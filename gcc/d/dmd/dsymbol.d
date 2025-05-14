/**
 * The base class for a D symbol, which can be a module, variable, function, enum, etc.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dsymbol.d, _dsymbol.d)
 * Documentation:  https://dlang.org/phobos/dmd_dsymbol.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dsymbol.d
 */

module dmd.dsymbol;

import core.stdc.stdarg;
import core.stdc.stdio;
import core.stdc.string;
import core.stdc.stdlib;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.attrib;
import dmd.astenums;
import dmd.ast_node;
import dmd.gluelayer;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dmodule;
import dmd.dversion;
import dmd.dscope;
import dmd.dstruct;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.lexer;
import dmd.location;
import dmd.mtype;
import dmd.nspace;
import dmd.root.aav;
import dmd.root.rmem;
import dmd.rootobject;
import dmd.root.string;
import dmd.statement;
import dmd.staticassert;
import dmd.tokens;
import dmd.visitor;
import dmd.dsymbolsem;

import dmd.common.outbuffer;

/***************************************
 * Calls dg(Dsymbol* sym) for each Dsymbol.
 * If dg returns !=0, stops and returns that value else returns 0.
 * Params:
 *    symbols = Dsymbols
 *    dg = delegate to call for each Dsymbol
 * Returns:
 *    last value returned by dg()
 *
 * See_Also: $(REF each, dmd, root, array)
 */
int foreachDsymbol(Dsymbols* symbols, scope int delegate(Dsymbol) dg)
{
    assert(dg);
    if (symbols)
    {
        /* Do not use foreach, as the size of the array may expand during iteration
         */
        for (size_t i = 0; i < symbols.length; ++i)
        {
            Dsymbol s = (*symbols)[i];
            const result = dg(s);
            if (result)
                return result;
        }
    }
    return 0;
}

/***************************************
 * Calls dg(Dsymbol* sym) for each Dsymbol.
 * Params:
 *    symbols = Dsymbols
 *    dg = delegate to call for each Dsymbol
 *
 * See_Also: $(REF each, dmd, root, array)
 */
void foreachDsymbol(Dsymbols* symbols, scope void delegate(Dsymbol) dg)
{
    assert(dg);
    if (symbols)
    {
        /* Do not use foreach, as the size of the array may expand during iteration
         */
        for (size_t i = 0; i < symbols.length; ++i)
        {
            Dsymbol s = (*symbols)[i];
            dg(s);
        }
    }
}


struct Ungag
{
    uint oldgag;

    extern (D) this(uint old) nothrow @safe
    {
        this.oldgag = old;
    }

    extern (C++) ~this() nothrow
    {
        global.gag = oldgag;
    }
}

struct Visibility
{
    ///
    enum Kind : ubyte
    {
        undefined,
        none,           // no access
        private_,
        package_,
        protected_,
        public_,
        export_,
    }

    Kind kind;
    Package pkg;

    extern(C++) this(Visibility.Kind kind, Package pkg = null) pure nothrow @nogc @safe
    {
        this.kind = kind;
        this.pkg  = pkg;
    }

    extern (D):

    /**
     * Checks if `this` is less or more visible than `other`
     *
     * Params:
     *   other = Visibility to compare `this` to.
     *
     * Returns:
     *   A value `< 0` if `this` is less visible than `other`,
     *   a value `> 0` if `this` is more visible than `other`,
     *   and `0` if they are at the same level.
     *   Note that `package` visibility with different packages
     *   will also return `0`.
     */
    int opCmp(const Visibility other) const pure nothrow @nogc @safe
    {
        return this.kind - other.kind;
    }

    ///
    unittest
    {
        assert(Visibility(Visibility.Kind.public_) > Visibility(Visibility.Kind.private_));
        assert(Visibility(Visibility.Kind.private_) < Visibility(Visibility.Kind.protected_));
        assert(Visibility(Visibility.Kind.package_) >= Visibility(Visibility.Kind.package_));
    }

    /**
     * Checks if `this` is absolutely identical visibility attribute to `other`
     */
    bool opEquals(ref const Visibility other) const @safe
    {
        if (this.kind == other.kind)
        {
            if (this.kind == Visibility.Kind.package_)
                return this.pkg == other.pkg;
            return true;
        }
        return false;
    }
}

enum PASS : ubyte
{
    initial,        // initial state
    semantic,       // semantic() started
    semanticdone,   // semantic() done
    semantic2,      // semantic2() started
    semantic2done,  // semantic2() done
    semantic3,      // semantic3() started
    semantic3done,  // semantic3() done
    inline,         // inline started
    inlinedone,     // inline done
    obj,            // toObjFile() run
}

// Search options
alias SearchOptFlags = uint;
enum SearchOpt : SearchOptFlags
{
    all                     = 0x00, // search for all symbols
    ignorePrivateImports    = 0x01, // don't search private imports
    ignoreErrors            = 0x02, // don't give error messages
    ignoreAmbiguous         = 0x04, // return NULL if ambiguous
    localsOnly              = 0x08, // only look at locals (don't search imports)
    importsOnly             = 0x10, // only look in imports
    unqualifiedModule       = 0x20, // the module scope search is unqualified,
                                    // meaning don't search imports in that scope,
                                    // because qualified module searches search
                                    // their imports
    tagNameSpace            = 0x40, // search ImportC tag symbol table
    ignoreVisibility        = 0x80, // also find private and package protected symbols
}

/***********************************************************
 * Struct/Class/Union field state.
 * Used for transitory information when setting field offsets, such
 * as bit fields.
 */
struct FieldState
{
    uint offset;        /// byte offset for next field

    uint fieldOffset;   /// byte offset for the start of the bit field
    uint fieldSize;     /// byte size of field
    uint fieldAlign;    /// byte alignment of field
    uint bitOffset;     /// bit offset for field

    bool inFlight;      /// bit field is in flight

    void print() const
    {
        printf("FieldState.offset      = %d bytes\n",   offset);
        printf("          .fieldOffset = %d bytes\n",   fieldOffset);
        printf("          .bitOffset   = %d bits\n",    bitOffset);
        printf("          .fieldSize   = %d bytes\n",   fieldSize);
        printf("          .inFlight    = %d\n",         inFlight);
    }
}

// 99.9% of Dsymbols don't have attributes (at least in druntime and Phobos),
// so save memory by grouping them into a separate struct
private struct DsymbolAttributes
{
    /// C++ namespace this symbol belongs to
    CPPNamespaceDeclaration cppnamespace;
    /// customized deprecation message
    DeprecatedDeclaration depdecl_;
    /// user defined attributes
    UserAttributeDeclaration userAttribDecl;
}

enum DSYM : ubyte
{
    none,
    dsymbol,
    linkDeclaration,
    cppMangleDeclaration,
    alignDeclaration,
    pragmaDeclaration,
    conditionalDeclaration,
    staticForeachDeclaration,
    userAttributeDeclaration,
    labelDsymbol,
    aliasThis,
    package_,
    module_,
    enumMember,
    templateDeclaration,
    templateInstance,
    templateMixin,
    forwardingAttribDeclaration,
    nspace,
    declaration,
    storageClassDeclaration,
    expressionDsymbol,
    aliasAssign,
    thisDeclaration,
    bitFieldDeclaration,
    typeInfoDeclaration,
    tupleDeclaration,
    aliasDeclaration,
    aggregateDeclaration,
    funcDeclaration,
    funcAliasDeclaration,
    overDeclaration,
    funcLiteralDeclaration,
    ctorDeclaration,
    postBlitDeclaration,
    dtorDeclaration,
    staticCtorDeclaration,
    staticDtorDeclaration,
    sharedStaticCtorDeclaration,
    sharedStaticDtorDeclaration,
    invariantDeclaration,
    unitTestDeclaration,
    newDeclaration,
    varDeclaration,
    versionSymbol,
    debugSymbol,
    classDeclaration,
    structDeclaration,
    unionDeclaration,
    interfaceDeclaration,
    scopeDsymbol,
    forwardingScopeDsymbol,
    withScopeSymbol,
    arrayScopeSymbol,
    import_,
    enumDeclaration,
    symbolDeclaration,
    attribDeclaration,
    anonDeclaration,
    cppNamespaceDeclaration,
    visibilityDeclaration,
    overloadSet,
    mixinDeclaration,
    staticAssert,
    staticIfDeclaration,
    cAsmDeclaration
}

/***********************************************************
 */
extern (C++) class Dsymbol : ASTNode
{
    Identifier ident;
    Dsymbol parent;
    Symbol* csym;           // symbol for code generator
    Scope* _scope;          // !=null means context to use for semantic()
    private DsymbolAttributes* atts; /// attached attribute declarations
    const Loc loc;          // where defined
    ushort localNum;        /// perturb mangled name to avoid collisions with those in FuncDeclaration.localsymtab
    static struct BitFields
    {
        bool errors;            // this symbol failed to pass semantic()
        PASS semanticRun = PASS.initial;
    }
    import dmd.common.bitfields;
    mixin(generateBitFields!(BitFields, ubyte));
    DSYM dsym;

    final extern (D) this(DSYM tag) nothrow @safe
    {
        //printf("Dsymbol::Dsymbol(%p)\n", this);
        this.dsym = tag;
        loc = Loc.initial;
    }

    final extern (D) this(DSYM tag, Identifier ident) nothrow @safe
    {
        //printf("Dsymbol::Dsymbol(%p, ident)\n", this);
        this.dsym = tag;
        this.loc = Loc.initial;
        this.ident = ident;
    }

    final extern (D) this(DSYM tag, Loc loc, Identifier ident) nothrow @safe
    {
        //printf("Dsymbol::Dsymbol(%p, ident)\n", this);
        this.dsym = tag;
        this.loc = loc;
        this.ident = ident;
    }

    static Dsymbol create(Identifier ident) nothrow @safe
    {
        return new Dsymbol(DSYM.dsymbol, ident);
    }

    final override const(char)* toChars() const
    {
        import dmd.hdrgen : toChars;
        return toChars(this);
    }

    // Getters / setters for fields stored in `DsymbolAttributes`
    final nothrow pure @safe
    {
        private ref DsymbolAttributes getAtts()
        {
            if (!atts)
                atts = new DsymbolAttributes();
            return *atts;
        }

        inout(DeprecatedDeclaration) depdecl() inout { return atts ? atts.depdecl_ : null; }
        inout(CPPNamespaceDeclaration) cppnamespace() inout { return atts ? atts.cppnamespace : null; }
        inout(UserAttributeDeclaration) userAttribDecl() inout { return atts ? atts.userAttribDecl : null; }

        DeprecatedDeclaration depdecl(DeprecatedDeclaration dd)
        {
            if (!dd && !atts)
                return null;
            return getAtts().depdecl_ = dd;
        }

        CPPNamespaceDeclaration cppnamespace(CPPNamespaceDeclaration ns)
        {
            if (!ns && !atts)
                return null;
            return getAtts().cppnamespace = ns;
        }

        UserAttributeDeclaration userAttribDecl(UserAttributeDeclaration uad)
        {
            if (!uad && !atts)
                return null;
            return getAtts().userAttribDecl = uad;
        }
    }

    // helper to print fully qualified (template) arguments
    const(char)* toPrettyCharsHelper()
    {
        return toChars();
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        const s = o.isDsymbol();
        if (!s)
            return false;
        // Overload sets don't have an ident
        // Function-local declarations may have identical names
        // if they are declared in different scopes
        if (s && ident && s.ident && ident.equals(s.ident) && localNum == s.localNum)
            return true;
        return false;
    }

    final bool isAnonymous() const
    {
        return ident is null || ident.isAnonymous;
    }

    extern(D) private const(char)[] prettyFormatHelper()
    {
        const cstr = toPrettyChars();
        return '`' ~ cstr.toDString() ~ "`\0";
    }

    /**********************************
     * Determine which Module a Dsymbol is in.
     */
    final Module getModule()
    {
        //printf("Dsymbol::getModule()\n");
        if (TemplateInstance ti = isInstantiated())
            return ti.tempdecl.getModule();
        Dsymbol s = this;
        while (s)
        {
            //printf("\ts = %s '%s'\n", s.kind(), s.toPrettyChars());
            if (Module m = s.isModule())
                return m;
            s = s.parent;
        }
        return null;
    }

    /**************************************
     * Does this Dsymbol come from a C file?
     * Returns:
     *  true if it does
     */
     final bool isCsymbol()
     {
        if (Module m = getModule())
            return m.filetype == FileType.c;
        return false;
    }

    /**********************************
     * Determine which Module a Dsymbol is in, as far as access rights go.
     */
    final Module getAccessModule()
    {
        //printf("Dsymbol::getAccessModule()\n");
        if (TemplateInstance ti = isInstantiated())
            return ti.tempdecl.getAccessModule();
        Dsymbol s = this;
        while (s)
        {
            //printf("\ts = %s '%s'\n", s.kind(), s.toPrettyChars());
            if (Module m = s.isModule())
                return m;
            TemplateInstance ti = s.isTemplateInstance();
            if (ti && ti.enclosing)
            {
                /* Because of local template instantiation, the parent isn't where the access
                 * rights come from - it's the template declaration
                 */
                s = ti.tempdecl;
            }
            else
                s = s.parent;
        }
        return null;
    }

    /**
     * `pastMixin` returns the enclosing symbol if this is a template mixin.
     *
     * `pastMixinAndNspace` does likewise, additionally skipping over Nspaces that
     * are mangleOnly.
     *
     * See also `parent`, `toParent` and `toParent2`.
     */
    final inout(Dsymbol) pastMixin() inout @safe
    {
        //printf("Dsymbol::pastMixin() %s\n", toChars());
        if (!isTemplateMixin() && !isForwardingAttribDeclaration() && !isForwardingScopeDsymbol())
            return this;
        if (!parent)
            return null;
        return parent.pastMixin();
    }

    /**********************************
     * `parent` field returns a lexically enclosing scope symbol this is a member of.
     *
     * `toParent()` returns a logically enclosing scope symbol this is a member of.
     * It skips over TemplateMixin's.
     *
     * `toParent2()` returns an enclosing scope symbol this is living at runtime.
     * It skips over both TemplateInstance's and TemplateMixin's.
     * It's used when looking for the 'this' pointer of the enclosing function/class.
     *
     * `toParentDecl()` similar to `toParent2()` but always follows the template declaration scope
     * instead of the instantiation scope.
     *
     * `toParentLocal()` similar to `toParentDecl()` but follows the instantiation scope
     * if a template declaration is non-local i.e. global or static.
     *
     * Examples:
     * ---
     *  module mod;
     *  template Foo(alias a) { mixin Bar!(); }
     *  mixin template Bar() {
     *    public {  // VisibilityDeclaration
     *      void baz() { a = 2; }
     *    }
     *  }
     *  void test() {
     *    int v = 1;
     *    alias foo = Foo!(v);
     *    foo.baz();
     *    assert(v == 2);
     *  }
     *
     *  // s == FuncDeclaration('mod.test.Foo!().Bar!().baz()')
     *  // s.parent == TemplateMixin('mod.test.Foo!().Bar!()')
     *  // s.toParent() == TemplateInstance('mod.test.Foo!()')
     *  // s.toParent2() == FuncDeclaration('mod.test')
     *  // s.toParentDecl() == Module('mod')
     *  // s.toParentLocal() == FuncDeclaration('mod.test')
     * ---
     */
    final inout(Dsymbol) toParent() inout @safe
    {
        return parent ? parent.pastMixin() : null;
    }

    /// ditto
    final inout(Dsymbol) toParent2() inout @safe
    {
        if (!parent || !parent.isTemplateInstance && !parent.isForwardingAttribDeclaration() && !parent.isForwardingScopeDsymbol())
            return parent;
        return parent.toParent2;
    }

    /// ditto
    final inout(Dsymbol) toParentDecl() inout
    {
        return toParentDeclImpl(false);
    }

    /// ditto
    final inout(Dsymbol) toParentLocal() inout
    {
        return toParentDeclImpl(true);
    }

    private inout(Dsymbol) toParentDeclImpl(bool localOnly) inout
    {
        auto p = toParent();
        if (!p || !p.isTemplateInstance())
            return p;
        auto ti = p.isTemplateInstance();
        if (ti.tempdecl && (!localOnly || !(cast(TemplateDeclaration)ti.tempdecl).isstatic))
            return ti.tempdecl.toParentDeclImpl(localOnly);
        return parent.toParentDeclImpl(localOnly);
    }

    /**
     * Returns the declaration scope scope of `this` unless any of the symbols
     * `p1` or `p2` resides in its enclosing instantiation scope then the
     * latter is returned.
     */
    final Dsymbol toParentP(Dsymbol p1, Dsymbol p2 = null)
    {
        return followInstantiationContext(p1, p2) ? toParent2() : toParentLocal();
    }

    final inout(TemplateInstance) isInstantiated() inout
    {
        if (!parent)
            return null;
        auto ti = parent.isTemplateInstance();
        if (ti && !ti.isTemplateMixin())
            return ti;
        return parent.isInstantiated();
    }

    /***
     * Returns true if any of the symbols `p1` or `p2` resides in the enclosing
     * instantiation scope of `this`.
     */
    final bool followInstantiationContext(Dsymbol p1, Dsymbol p2 = null)
    {
        static bool has2This(Dsymbol s)
        {
            if (auto f = s.isFuncDeclaration())
                return f.hasDualContext();
            if (auto ad = s.isAggregateDeclaration())
                return ad.vthis2 !is null;
            return false;
        }

        if (has2This(this))
        {
            assert(p1);
            auto outer = toParent();
            while (outer)
            {
                auto ti = outer.isTemplateInstance();
                if (!ti)
                    break;
                foreach (oarg; *ti.tiargs)
                {
                    auto sa = getDsymbol(oarg);
                    if (!sa)
                        continue;
                    sa = sa.toAlias().toParent2();
                    if (!sa)
                        continue;
                    if (sa == p1)
                        return true;
                    if (p2 && sa == p2)
                        return true;
                }
                outer = ti.tempdecl.toParent();
            }
            return false;
        }
        return false;
    }

    // Check if this function is a member of a template which has only been
    // instantiated speculatively, eg from inside is(typeof()).
    // Return the speculative template instance it is part of,
    // or NULL if not speculative.
    final inout(TemplateInstance) isSpeculative() inout
    {
        if (!parent)
            return null;
        auto ti = parent.isTemplateInstance();
        if (ti && ti.gagged)
            return ti;
        if (!parent.toParent())
            return null;
        return parent.isSpeculative();
    }

    final Ungag ungagSpeculative() const
    {
        const oldgag = global.gag;
        if (global.gag && !isSpeculative() && !toParent2().isFuncDeclaration())
            global.gag = 0;
        return Ungag(oldgag);
    }

    // kludge for template.isSymbol()
    override final DYNCAST dyncast() const
    {
        return DYNCAST.dsymbol;
    }

    /*************************************
     * Do syntax copy of an array of Dsymbol's.
     */
    extern (D) static Dsymbols* arraySyntaxCopy(Dsymbols* a)
    {
        Dsymbols* b = null;
        if (a)
        {
            b = a.copy();
            for (size_t i = 0; i < b.length; i++)
            {
                (*b)[i] = (*b)[i].syntaxCopy(null);
            }
        }
        return b;
    }

    Identifier getIdent()
    {
        return ident;
    }

    const(char)* toPrettyChars(bool QualifyTypes = false)
    {
        //printf("Dsymbol::toPrettyChars() '%s'\n", toChars());
        if (!parent)
        {
            auto s = toChars();
            return s;
        }

        OutBuffer buf;

        void addQualifiers(Dsymbol p)
        {
            if (p.parent)
            {
                addQualifiers(p.parent);
                buf.writeByte('.');
            }
            const s = QualifyTypes ? p.toPrettyCharsHelper() : p.toChars();
            buf.writestring(s);
        }

        addQualifiers(this);
        auto s = buf.extractSlice(true).ptr;

        return s;
    }

    const(char)* kind() const pure nothrow @nogc @safe
    {
        return "symbol";
    }

    /*********************************
     * If this symbol is really an alias for another,
     * return that other.
     * If needed, semantic() is invoked due to resolve forward reference.
     */
    Dsymbol toAlias()
    {
        return this;
    }

    /*********************************
     * Resolve recursive tuple expansion in eponymous template.
     */
    Dsymbol toAlias2()
    {
        return toAlias();
    }

    bool overloadInsert(Dsymbol s)
    {
        //printf("Dsymbol::overloadInsert('%s')\n", s.toChars());
        return false;
    }

    /*********************************
     * Returns:
     *  SIZE_INVALID when the size cannot be determined
     */
    uinteger_t size(Loc loc)
    {
        .error(loc, "%s `%s` symbol `%s` has no size", kind, toPrettyChars, toChars());
        return SIZE_INVALID;
    }

    bool isforwardRef()
    {
        return false;
    }

    // is a 'this' required to access the member
    inout(AggregateDeclaration) isThis() inout
    {
        return null;
    }

    // is Dsymbol exported?
    bool isExport() const
    {
        return false;
    }

    // is Dsymbol imported?
    bool isImportedSymbol() const
    {
        return false;
    }

    // is Dsymbol deprecated?
    bool isDeprecated() @safe @nogc pure nothrow const
    {
        return false;
    }

    bool isOverloadable() const
    {
        return false;
    }

    // is this a LabelDsymbol()?
    LabelDsymbol isLabel()
    {
        return null;
    }

    /// Returns an AggregateDeclaration when toParent() is that.
    final inout(AggregateDeclaration) isMember() inout
    {
        //printf("Dsymbol::isMember() %s\n", toChars());
        auto p = toParent();
        //printf("parent is %s %s\n", p.kind(), p.toChars());
        return p ? p.isAggregateDeclaration() : null;
    }

    /// Returns an AggregateDeclaration when toParent2() is that.
    final inout(AggregateDeclaration) isMember2() inout
    {
        //printf("Dsymbol::isMember2() '%s'\n", toChars());
        auto p = toParent2();
        //printf("parent is %s %s\n", p.kind(), p.toChars());
        return p ? p.isAggregateDeclaration() : null;
    }

    /// Returns an AggregateDeclaration when toParentDecl() is that.
    final inout(AggregateDeclaration) isMemberDecl() inout
    {
        //printf("Dsymbol::isMemberDecl() '%s'\n", toChars());
        auto p = toParentDecl();
        //printf("parent is %s %s\n", p.kind(), p.toChars());
        return p ? p.isAggregateDeclaration() : null;
    }

    /// Returns an AggregateDeclaration when toParentLocal() is that.
    final inout(AggregateDeclaration) isMemberLocal() inout
    {
        //printf("Dsymbol::isMemberLocal() '%s'\n", toChars());
        auto p = toParentLocal();
        //printf("parent is %s %s\n", p.kind(), p.toChars());
        return p ? p.isAggregateDeclaration() : null;
    }

    // is this a member of a ClassDeclaration?
    final ClassDeclaration isClassMember()
    {
        auto ad = isMember();
        return ad ? ad.isClassDeclaration() : null;
    }

    // is this a type?
    Type getType()
    {
        return null;
    }

    // need a 'this' pointer?
    bool needThis()
    {
        return false;
    }

    /*************************************
     */
    Visibility visible() pure nothrow @nogc @safe
    {
        return Visibility(Visibility.Kind.public_);
    }

    /**************************************
     * Copy the syntax.
     * Used for template instantiations.
     * If s is NULL, allocate the new object, otherwise fill it in.
     */
    Dsymbol syntaxCopy(Dsymbol s)
    {
        printf("%s %s\n", kind(), toChars());
        assert(0);
    }

    /*****************************************
     * Same as Dsymbol::oneMember(), but look at an array of Dsymbols.
     */
    extern (D) static bool oneMembers(Dsymbols* members, out Dsymbol ps, Identifier ident)
    {
        //printf("Dsymbol::oneMembers() %d\n", members ? members.length : 0);
        Dsymbol s = null;
        if (!members)
        {
            ps = null;
            return true;
        }

        for (size_t i = 0; i < members.length; i++)
        {
            Dsymbol sx = (*members)[i];
            bool x = sx.oneMember(ps, ident); //MYTODO: this temporarily creates a new dependency to dsymbolsem, will need to extract oneMembers() later
            //printf("\t[%d] kind %s = %d, s = %p\n", i, sx.kind(), x, *ps);
            if (!x)
            {
                //printf("\tfalse 1\n");
                assert(ps is null);
                return false;
            }
            if (ps)
            {
                assert(ident);
                if (!ps.ident || !ps.ident.equals(ident))
                    continue;
                if (!s)
                    s = ps;
                else if (s.isOverloadable() && ps.isOverloadable())
                {
                    // keep head of overload set
                    FuncDeclaration f1 = s.isFuncDeclaration();
                    FuncDeclaration f2 = ps.isFuncDeclaration();
                    if (f1 && f2)
                    {
                        assert(!f1.isFuncAliasDeclaration());
                        assert(!f2.isFuncAliasDeclaration());
                        for (; f1 != f2; f1 = f1.overnext0)
                        {
                            if (f1.overnext0 is null)
                            {
                                f1.overnext0 = f2;
                                break;
                            }
                        }
                    }
                }
                else // more than one symbol
                {
                    ps = null;
                    //printf("\tfalse 2\n");
                    return false;
                }
            }
        }
        ps = s; // s is the one symbol, null if none
        //printf("\ttrue\n");
        return true;
    }

    /*****************************************
     * Is Dsymbol a variable that contains pointers?
     */
    bool hasPointers()
    {
        //printf("Dsymbol::hasPointers() %s\n", toChars());
        return false;
    }

    void addObjcSymbols(ClassDeclarations* classes, ClassDeclarations* categories)
    {
    }

    /****************************************
     * Add documentation comment to Dsymbol.
     * Ignore NULL comments.
     */
    void addComment(const(char)* comment)
    {
        import dmd.dsymbolsem;
        dmd.dsymbolsem.addComment(this, comment);
    }

    /// get documentation comment for this Dsymbol
    final const(char)* comment()
    {
        //printf("getcomment: %p '%s'\n", this, this.toChars());
        if (auto p = cast(void*)this in commentHashTable)
        {
            //printf("comment: '%s'\n", *p);
            return *p;
        }
        return null;
    }

    /* Shell around addComment() to avoid disruption for the moment */
    final void comment(const(char)* comment) { addComment(comment); }

    extern (D) __gshared const(char)*[void*] commentHashTable;


    /**********************************
     * Get ddoc unittest associated with this symbol.
     * (only use this with ddoc)
     * Returns: ddoc unittest, null if none
     */
    final UnitTestDeclaration ddocUnittest()
    {
        if (auto p = cast(void*)this in ddocUnittestHashTable)
            return *p;
        return null;
    }

    /**********************************
     * Set ddoc unittest associated with this symbol.
     */
    final void ddocUnittest(UnitTestDeclaration utd)
    {
        ddocUnittestHashTable[cast(void*)this] = utd;
    }

    private extern (D) __gshared UnitTestDeclaration[void*] ddocUnittestHashTable;


    /****************************************
     * Returns true if this symbol is defined in a non-root module without instantiation.
     */
    final bool inNonRoot()
    {
        Dsymbol s = parent;
        for (; s; s = s.toParent())
        {
            if (auto ti = s.isTemplateInstance())
            {
                return false;
            }
            if (auto m = s.isModule())
            {
                if (!m.isRoot())
                    return true;
                break;
            }
        }
        return false;
    }

    /**
     * Deinitializes the global state of the compiler.
     *
     * This can be used to restore the state set by `_init` to its original
     * state.
     */
    static void deinitialize()
    {
        commentHashTable = commentHashTable.init;
        ddocUnittestHashTable = ddocUnittestHashTable.init;
    }

    /************
     */
    override void accept(Visitor v)
    {
        v.visit(this);
    }

  pure nothrow @trusted @nogc final:

    inout(Package)                     isPackage()                     inout { return (dsym == DSYM.package_ || dsym == DSYM.module_) ? cast(inout(Package)) cast(void*) this : null; }
    inout(Module)                      isModule()                      inout { return dsym == DSYM.module_ ? cast(inout(Module)) cast(void*) this : null; }
    inout(EnumMember)                  isEnumMember()                  inout { return dsym == DSYM.enumMember ? cast(inout(EnumMember)) cast(void*) this : null; }
    inout(TemplateDeclaration)         isTemplateDeclaration()         inout { return dsym == DSYM.templateDeclaration ? cast(inout(TemplateDeclaration)) cast(void*) this : null; }
    inout(TemplateInstance)            isTemplateInstance()            inout { return (dsym == DSYM.templateInstance || dsym == DSYM.templateMixin) ? cast(inout(TemplateInstance)) cast(void*) this : null; }
    inout(TemplateMixin)               isTemplateMixin()               inout { return dsym == DSYM.templateMixin ? cast(inout(TemplateMixin)) cast(void*) this : null; }
    inout(ForwardingAttribDeclaration) isForwardingAttribDeclaration() inout { return dsym == DSYM.forwardingAttribDeclaration ? cast(inout(ForwardingAttribDeclaration)) cast(void*) this : null; }
    inout(Nspace)                      isNspace()                      inout { return dsym == DSYM.nspace ? cast(inout(Nspace)) cast(void*) this : null; }
    inout(Declaration)                 isDeclaration()                 inout {
        switch (dsym)
        {
        case DSYM.tupleDeclaration:
        case DSYM.aliasDeclaration:
        case DSYM.overDeclaration:
        case DSYM.varDeclaration:
            case DSYM.bitFieldDeclaration:
            case DSYM.typeInfoDeclaration:
            case DSYM.thisDeclaration:
            case DSYM.enumMember:
        case DSYM.symbolDeclaration:
        case DSYM.funcDeclaration:
            case DSYM.funcAliasDeclaration:
            case DSYM.funcLiteralDeclaration:
            case DSYM.ctorDeclaration:
            case DSYM.postBlitDeclaration:
            case DSYM.dtorDeclaration:
            case DSYM.staticCtorDeclaration:
                case DSYM.sharedStaticCtorDeclaration:
            case DSYM.staticDtorDeclaration:
                case DSYM.sharedStaticDtorDeclaration:
            case DSYM.invariantDeclaration:
            case DSYM.unitTestDeclaration:
            case DSYM.newDeclaration:
            return cast(inout(Declaration)) cast(void*) this;
        default:
            return null;
        }
    }
    inout(StorageClassDeclaration)     isStorageClassDeclaration()     inout { return dsym == DSYM.storageClassDeclaration ? cast(inout(StorageClassDeclaration)) cast(void*) this : null; }
    inout(ExpressionDsymbol)           isExpressionDsymbol()           inout { return dsym == DSYM.expressionDsymbol ? cast(inout(ExpressionDsymbol)) cast(void*) this : null; }
    inout(AliasAssign)                 isAliasAssign()                 inout { return dsym == DSYM.aliasAssign ? cast(inout(AliasAssign)) cast(void*) this : null; }
    inout(ThisDeclaration)             isThisDeclaration()             inout { return dsym == DSYM.thisDeclaration ? cast(inout(ThisDeclaration)) cast(void*) this : null; }
    inout(BitFieldDeclaration)         isBitFieldDeclaration()         inout { return dsym == DSYM.bitFieldDeclaration ? cast(inout(BitFieldDeclaration)) cast(void*) this : null; }
    inout(TypeInfoDeclaration)         isTypeInfoDeclaration()         inout { return dsym == DSYM.typeInfoDeclaration ? cast(inout(TypeInfoDeclaration)) cast(void*) this : null; }
    inout(TupleDeclaration)            isTupleDeclaration()            inout { return dsym == DSYM.tupleDeclaration ? cast(inout(TupleDeclaration)) cast(void*) this : null; }
    inout(AliasDeclaration)            isAliasDeclaration()            inout { return dsym == DSYM.aliasDeclaration ? cast(inout(AliasDeclaration)) cast(void*) this : null; }
    inout(AggregateDeclaration)        isAggregateDeclaration()        inout {
        switch (dsym)
        {
        case DSYM.aggregateDeclaration:
        case DSYM.structDeclaration:
        case DSYM.unionDeclaration:
        case DSYM.classDeclaration:
        case DSYM.interfaceDeclaration:
            return cast(inout(AggregateDeclaration)) cast(void*) this;
        default:
            return null;
        }
    }
    inout(FuncDeclaration)             isFuncDeclaration()             inout {
        switch (dsym)
        {
        case DSYM.funcDeclaration:
            case DSYM.funcAliasDeclaration:
            case DSYM.funcLiteralDeclaration:
            case DSYM.ctorDeclaration:
            case DSYM.postBlitDeclaration:
            case DSYM.dtorDeclaration:
            case DSYM.staticCtorDeclaration:
                case DSYM.sharedStaticCtorDeclaration:
            case DSYM.staticDtorDeclaration:
                case DSYM.sharedStaticDtorDeclaration:
            case DSYM.invariantDeclaration:
            case DSYM.unitTestDeclaration:
            case DSYM.newDeclaration:
            return cast(inout(FuncDeclaration)) cast(void*) this;
        default:
            return null;
        }
    }
    inout(FuncAliasDeclaration)        isFuncAliasDeclaration()        inout { return dsym == DSYM.funcAliasDeclaration ? cast(inout(FuncAliasDeclaration)) cast(void*) this : null; }
    inout(OverDeclaration)             isOverDeclaration()             inout { return dsym == DSYM.overDeclaration ? cast(inout(OverDeclaration)) cast(void*) this : null; }
    inout(FuncLiteralDeclaration)      isFuncLiteralDeclaration()      inout { return dsym == DSYM.funcLiteralDeclaration ? cast(inout(FuncLiteralDeclaration)) cast(void*) this : null; }
    inout(CtorDeclaration)             isCtorDeclaration()             inout { return dsym == DSYM.ctorDeclaration ? cast(inout(CtorDeclaration)) cast(void*) this : null; }
    inout(PostBlitDeclaration)         isPostBlitDeclaration()         inout { return dsym == DSYM.postBlitDeclaration ? cast(inout(PostBlitDeclaration)) cast(void*) this : null; }
    inout(DtorDeclaration)             isDtorDeclaration()             inout { return dsym == DSYM.dtorDeclaration ? cast(inout(DtorDeclaration)) cast(void*) this : null; }
    inout(StaticCtorDeclaration)       isStaticCtorDeclaration()       inout { return (dsym == DSYM.staticCtorDeclaration || dsym == DSYM.sharedStaticCtorDeclaration) ? cast(inout(StaticCtorDeclaration)) cast(void*) this : null; }
    inout(StaticDtorDeclaration)       isStaticDtorDeclaration()       inout { return (dsym == DSYM.staticDtorDeclaration || dsym == DSYM.sharedStaticDtorDeclaration) ? cast(inout(StaticDtorDeclaration)) cast(void*) this : null; }
    inout(SharedStaticCtorDeclaration) isSharedStaticCtorDeclaration() inout { return dsym == DSYM.sharedStaticCtorDeclaration ? cast(inout(SharedStaticCtorDeclaration)) cast(void*) this : null; }
    inout(SharedStaticDtorDeclaration) isSharedStaticDtorDeclaration() inout { return dsym == DSYM.sharedStaticDtorDeclaration ? cast(inout(SharedStaticDtorDeclaration)) cast(void*) this : null; }
    inout(InvariantDeclaration)        isInvariantDeclaration()        inout { return dsym == DSYM.invariantDeclaration ? cast(inout(InvariantDeclaration)) cast(void*) this : null; }
    inout(UnitTestDeclaration)         isUnitTestDeclaration()         inout { return dsym == DSYM.unitTestDeclaration ? cast(inout(UnitTestDeclaration)) cast(void*) this : null; }
    inout(NewDeclaration)              isNewDeclaration()              inout { return dsym == DSYM.newDeclaration ? cast(inout(NewDeclaration)) cast(void*) this : null; }
    inout(VarDeclaration)              isVarDeclaration()              inout {
        switch (dsym)
        {
        case DSYM.varDeclaration:
            case DSYM.bitFieldDeclaration:
            case DSYM.typeInfoDeclaration:
            case DSYM.thisDeclaration:
            case DSYM.enumMember:
            return cast(inout(VarDeclaration)) cast(void*) this;
        default:
            return null;
        }
    }
    inout(VersionSymbol)               isVersionSymbol()               inout { return dsym == DSYM.versionSymbol ? cast(inout(VersionSymbol)) cast(void*) this : null; }
    inout(DebugSymbol)                 isDebugSymbol()                 inout { return dsym == DSYM.debugSymbol ? cast(inout(DebugSymbol)) cast(void*) this : null; }
    inout(ClassDeclaration)            isClassDeclaration()            inout { return (dsym == DSYM.classDeclaration || dsym == DSYM.interfaceDeclaration) ? cast(inout(ClassDeclaration)) cast(void*) this : null; }
    inout(StructDeclaration)           isStructDeclaration()           inout { return (dsym == DSYM.structDeclaration || dsym == DSYM.unionDeclaration) ? cast(inout(StructDeclaration)) cast(void*) this : null; }
    inout(UnionDeclaration)            isUnionDeclaration()            inout { return dsym == DSYM.unionDeclaration ? cast(inout(UnionDeclaration)) cast(void*) this : null; }
    inout(InterfaceDeclaration)        isInterfaceDeclaration()        inout { return dsym == DSYM.interfaceDeclaration ? cast(inout(InterfaceDeclaration)) cast(void*) this : null; }
    inout(ScopeDsymbol)                isScopeDsymbol()                inout {
        switch (dsym)
        {
        case DSYM.enumDeclaration:
        case DSYM.scopeDsymbol:
        case DSYM.package_:
            case DSYM.module_:
        case DSYM.nspace:
        case DSYM.templateInstance:
            case DSYM.templateMixin:
        case DSYM.templateDeclaration:
        case DSYM.aggregateDeclaration:
            case DSYM.structDeclaration:
                case DSYM.unionDeclaration:
            case DSYM.classDeclaration:
                case DSYM.interfaceDeclaration:
        case DSYM.withScopeSymbol:
        case DSYM.arrayScopeSymbol:
        case DSYM.forwardingScopeDsymbol:
            return cast(inout(ScopeDsymbol)) cast(void*) this;
        default:
            return null;
        }
    }
    inout(ForwardingScopeDsymbol)      isForwardingScopeDsymbol()      inout { return dsym == DSYM.forwardingScopeDsymbol ? cast(inout(ForwardingScopeDsymbol)) cast(void*) this : null; }
    inout(WithScopeSymbol)             isWithScopeSymbol()             inout { return dsym == DSYM.withScopeSymbol ? cast(inout(WithScopeSymbol)) cast(void*) this : null; }
    inout(ArrayScopeSymbol)            isArrayScopeSymbol()            inout { return dsym == DSYM.arrayScopeSymbol ? cast(inout(ArrayScopeSymbol)) cast(void*) this : null; }
    inout(Import)                      isImport()                      inout { return dsym == DSYM.import_ ? cast(inout(Import)) cast(void*) this : null; }
    inout(EnumDeclaration)             isEnumDeclaration()             inout { return dsym == DSYM.enumDeclaration ? cast(inout(EnumDeclaration)) cast(void*) this : null; }
    inout(SymbolDeclaration)           isSymbolDeclaration()           inout { return dsym == DSYM.symbolDeclaration ? cast(inout(SymbolDeclaration)) cast(void*) this : null; }
    inout(AttribDeclaration)           isAttribDeclaration()           inout {
        switch (dsym)
        {
        case DSYM.attribDeclaration:
        case DSYM.storageClassDeclaration:
        case DSYM.linkDeclaration:
        case DSYM.cppMangleDeclaration:
        case DSYM.cppNamespaceDeclaration:
        case DSYM.visibilityDeclaration:
        case DSYM.alignDeclaration:
        case DSYM.anonDeclaration:
        case DSYM.pragmaDeclaration:
        case DSYM.conditionalDeclaration:
            case DSYM.staticIfDeclaration:
        case DSYM.staticForeachDeclaration:
        case DSYM.forwardingAttribDeclaration:
        case DSYM.mixinDeclaration:
        case DSYM.userAttributeDeclaration:
            return cast(inout(AttribDeclaration)) cast(void*) this;
        default:
            return null;
        }
    }
    inout(AnonDeclaration)             isAnonDeclaration()             inout { return dsym == DSYM.anonDeclaration ? cast(inout(AnonDeclaration)) cast(void*) this : null; }
    inout(CPPNamespaceDeclaration)     isCPPNamespaceDeclaration()     inout { return dsym == DSYM.cppNamespaceDeclaration ? cast(inout(CPPNamespaceDeclaration)) cast(void*) this : null; }
    inout(VisibilityDeclaration)       isVisibilityDeclaration()       inout { return dsym == DSYM.visibilityDeclaration ? cast(inout(VisibilityDeclaration)) cast(void*) this : null; }
    inout(OverloadSet)                 isOverloadSet()                 inout { return dsym == DSYM.overloadSet ? cast(inout(OverloadSet)) cast(void*) this : null; }
    inout(MixinDeclaration)            isMixinDeclaration()            inout { return dsym == DSYM.mixinDeclaration ? cast(inout(MixinDeclaration)) cast(void*) this : null; }
    inout(StaticAssert)                isStaticAssert()                inout { return dsym == DSYM.staticAssert ? cast(inout(StaticAssert)) cast(void*) this : null; }
    inout(StaticIfDeclaration)         isStaticIfDeclaration()         inout { return dsym == DSYM.staticIfDeclaration ? cast(inout(StaticIfDeclaration)) cast(void*) this : null; }
    inout(CAsmDeclaration)             isCAsmDeclaration()             inout { return dsym == DSYM.cAsmDeclaration ? cast(inout(CAsmDeclaration)) cast(void*) this : null; }
}

/***********************************************************
 * Dsymbol that generates a scope
 */
extern (C++) class ScopeDsymbol : Dsymbol
{
    Dsymbols* members;          // all Dsymbol's in this scope
    DsymbolTable symtab;        // members[] sorted into table
    uint endlinnum;             // the linnumber of the statement after the scope (0 if unknown)
    /// symbols whose members have been imported, i.e. imported modules and template mixins
    Dsymbols* importedScopes;
    Visibility.Kind* visibilities; // array of Visibility.Kind, one for each import

private:

    import dmd.root.bitarray;
    BitArray accessiblePackages, privateAccessiblePackages;// whitelists of accessible (imported) packages

public:
    final extern (D) this() nothrow @safe
    {
        super(DSYM.scopeDsymbol);
    }

    final extern (D) this(Identifier ident) nothrow @safe
    {
        super(DSYM.scopeDsymbol, ident);
    }

    final extern (D) this(Loc loc, Identifier ident) nothrow @safe
    {
        super(DSYM.scopeDsymbol, loc, ident);
    }

    override ScopeDsymbol syntaxCopy(Dsymbol s)
    {
        //printf("ScopeDsymbol::syntaxCopy('%s')\n", toChars());
        ScopeDsymbol sds = s ? cast(ScopeDsymbol)s : new ScopeDsymbol(ident);
        sds.comment = comment;
        sds.members = arraySyntaxCopy(members);
        sds.endlinnum = endlinnum;
        return sds;
    }

    extern (D) final OverloadSet mergeOverloadSet(Identifier ident, OverloadSet os, Dsymbol s)
    {
        if (!os)
        {
            os = new OverloadSet(ident);
            os.parent = this;
        }
        if (OverloadSet os2 = s.isOverloadSet())
        {
            // Merge the cross-module overload set 'os2' into 'os'
            if (os.a.length == 0)
            {
                os.a.setDim(os2.a.length);
                memcpy(os.a.tdata(), os2.a.tdata(), (os.a[0]).sizeof * os2.a.length);
            }
            else
            {
                for (size_t i = 0; i < os2.a.length; i++)
                {
                    os = mergeOverloadSet(ident, os, os2.a[i]);
                }
            }
        }
        else
        {
            assert(s.isOverloadable());
            /* Don't add to os[] if s is alias of previous sym
             */
            for (size_t j = 0; j < os.a.length; j++)
            {
                Dsymbol s2 = os.a[j];
                if (s.toAlias() == s2.toAlias())
                {
                    if (s2.isDeprecated() || (s2.visible() < s.visible() && s.visible().kind != Visibility.Kind.none))
                    {
                        os.a[j] = s;
                    }
                    goto Lcontinue;
                }
            }
            os.push(s);
        Lcontinue:
        }
        return os;
    }

    void importScope(Dsymbol s, Visibility visibility) nothrow
    {
        //printf("%s.ScopeDsymbol::importScope(%s, %d)\n", toChars(), s.toChars(), visibility);
        // No circular or redundant import's
        if (s != this)
        {
            if (!importedScopes)
                importedScopes = new Dsymbols();
            else
            {
                for (size_t i = 0; i < importedScopes.length; i++)
                {
                    Dsymbol ss = (*importedScopes)[i];
                    if (ss == s) // if already imported
                    {
                        if (visibility.kind > visibilities[i])
                            visibilities[i] = visibility.kind; // upgrade access
                        return;
                    }
                }
            }
            importedScopes.push(s);
            visibilities = cast(Visibility.Kind*)mem.xrealloc(visibilities, importedScopes.length * (visibilities[0]).sizeof);
            visibilities[importedScopes.length - 1] = visibility.kind;
        }
    }


    /*****************************************
     * Returns: the symbols whose members have been imported, i.e. imported modules
     * and template mixins.
     *
     * See_Also: importScope
     */
    extern (D) final Dsymbols* getImportedScopes() nothrow @nogc @safe pure
    {
        return importedScopes;
    }

    /*****************************************
     * Returns: the array of visibilities associated with each imported scope. The
     * length of the array matches the imported scopes array.
     *
     * See_Also: getImportedScopes
     */
    extern (D) final Visibility.Kind[] getImportVisibilities() nothrow @nogc @safe pure
    {
        if (!importedScopes)
            return null;

        return (() @trusted => visibilities[0 .. importedScopes.length])();
    }

    extern (D) final void addAccessiblePackage(Package p, Visibility visibility) nothrow
    {
        auto pary = visibility.kind == Visibility.Kind.private_ ? &privateAccessiblePackages : &accessiblePackages;
        if (pary.length <= p.tag)
            pary.length = p.tag + 1;
        (*pary)[p.tag] = true;
    }

    bool isPackageAccessible(Package p, Visibility visibility, SearchOptFlags flags = SearchOpt.all) nothrow
    {
        if (p.tag < accessiblePackages.length && accessiblePackages[p.tag] ||
            visibility.kind == Visibility.Kind.private_ && p.tag < privateAccessiblePackages.length && privateAccessiblePackages[p.tag])
            return true;
        foreach (i, ss; importedScopes ? (*importedScopes)[] : null)
        {
            // only search visible scopes && imported modules should ignore private imports
            if (visibility.kind <= visibilities[i] &&
                ss.isScopeDsymbol.isPackageAccessible(p, visibility, SearchOpt.ignorePrivateImports))
                return true;
        }
        return false;
    }

    override final bool isforwardRef() nothrow
    {
        return (members is null);
    }

    static void multiplyDefined(Loc loc, Dsymbol s1, Dsymbol s2)
    {
        version (none)
        {
            printf("ScopeDsymbol::multiplyDefined()\n");
            printf("s1 = %p, '%s' kind = '%s', parent = %s\n", s1, s1.toChars(), s1.kind(), s1.parent ? s1.parent.toChars() : "");
            printf("s2 = %p, '%s' kind = '%s', parent = %s\n", s2, s2.toChars(), s2.kind(), s2.parent ? s2.parent.toChars() : "");
        }
        if (loc.isValid())
        {
            .error(loc, "`%s` matches conflicting symbols:", s1.ident.toChars());
            errorSupplemental(s1.loc, "%s `%s`", s1.kind(), s1.toPrettyChars());
            errorSupplemental(s2.loc, "%s `%s`", s2.kind(), s2.toPrettyChars());

            static if (0)
            {
                if (auto so = s1.isOverloadSet())
                {
                    printf("first %p:\n", so);
                    foreach (s; so.a[])
                    {
                        printf("  %p %s `%s` at %s\n", s, s.kind(), s.toPrettyChars(), s.locToChars());
                    }
                }
                if (auto so = s2.isOverloadSet())
                {
                    printf("second %p:\n", so);
                    foreach (s; so.a[])
                    {
                        printf("  %p %s `%s` at %s\n", s, s.kind(), s.toPrettyChars(), s.locToChars());
                    }
                }
            }
        }
        else
        {
            .error(s1.loc, "%s `%s` conflicts with %s `%s` at %s", s1.kind, s1.toPrettyChars, s2.kind(), s2.toPrettyChars(), s2.loc.toChars());
        }
    }

    override const(char)* kind() const
    {
        return "ScopeDsymbol";
    }

    /********************************
     * Insert Dsymbol in table.
     * Params:
     *   s = symbol to add
     * Returns:
     *   null if already in table, `s` if inserted
     */
    Dsymbol symtabInsert(Dsymbol s) nothrow
    {
        return symtab.insert(s);
    }

    /****************************************
     * Look up identifier in symbol table.
     * Params:
     *  s = symbol
     *  id = identifier to look up
     * Returns:
     *   Dsymbol if found, null if not
     */
    Dsymbol symtabLookup(Dsymbol s, Identifier id) nothrow
    {
        return symtab.lookup(id);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * With statement scope
 */
extern (C++) final class WithScopeSymbol : ScopeDsymbol
{
    WithStatement withstate;

    extern (D) this(WithStatement withstate) nothrow @safe
    {
        this.withstate = withstate;
        this.dsym = DSYM.withScopeSymbol;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Array Index/Slice scope
 */
extern (C++) final class ArrayScopeSymbol : ScopeDsymbol
{
    // either a SliceExp, an IndexExp, an ArrayExp, a TypeTuple or a TupleDeclaration.
    // Discriminated using DYNCAST and, for expressions, also EXP
    RootObject arrayContent;

    extern (D) this(Scope* sc, Expression exp) nothrow @safe
    {
        super(exp.loc, null);
        assert(exp.op == EXP.index || exp.op == EXP.slice || exp.op == EXP.array);
        this._scope = sc;
        this.arrayContent = exp;
        this.dsym = DSYM.arrayScopeSymbol;
    }

    extern (D) this(Scope* sc, TypeTuple type) nothrow @safe
    {
        this._scope = sc;
        this.arrayContent = type;
    }

    extern (D) this(Scope* sc, TupleDeclaration td) nothrow @safe
    {
        this._scope = sc;
        this.arrayContent = td;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Overload Sets
 */
extern (C++) final class OverloadSet : Dsymbol
{
    Dsymbols a;     // array of Dsymbols

    extern (D) this(Identifier ident, OverloadSet os = null) nothrow
    {
        super(DSYM.overloadSet, ident);
        if (os)
        {
            a.pushSlice(os.a[]);
        }
    }

    void push(Dsymbol s) nothrow
    {
        a.push(s);
    }

    override const(char)* kind() const
    {
        return "overloadset";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Forwarding ScopeDsymbol.  Used by ForwardingAttribDeclaration and
 * ForwardingScopeDeclaration to forward symbol insertions to another
 * scope.  See `dmd.attrib.ForwardingAttribDeclaration` for more
 * details.
 */
extern (C++) final class ForwardingScopeDsymbol : ScopeDsymbol
{
    extern (D) this() nothrow @safe
    {
        super();
        this.dsym = DSYM.forwardingScopeDsymbol;
    }

    override Dsymbol symtabInsert(Dsymbol s) nothrow
    {
        if (auto d = s.isDeclaration())
        {
            if (d.storage_class & STC.local)
            {
                // Symbols with storage class STC.local are not
                // forwarded, but stored in the local symbol
                // table. (Those are the `static foreach` variables.)
                if (!symtab)
                {
                    symtab = new DsymbolTable();
                }
                return super.symtabInsert(s); // insert locally
            }
        }
        auto forward = parent.isScopeDsymbol();
        assert(forward);
        if (!forward.symtab)
        {
            forward.symtab = new DsymbolTable();
        }
        // Non-STC.local symbols are forwarded to `forward`.
        return forward.symtabInsert(s);
    }

    /************************
     * This override handles the following two cases:
     *     static foreach (i, i; [0]) { ... }
     * and
     *     static foreach (i; [0]) { enum i = 2; }
     */
    override Dsymbol symtabLookup(Dsymbol s, Identifier id) nothrow
    {
        // correctly diagnose clashing foreach loop variables.
        if (auto d = s.isDeclaration())
        {
            if (d.storage_class & STC.local)
            {
                if (!symtab)
                {
                    symtab = new DsymbolTable();
                }
                return super.symtabLookup(s,id);
            }
        }
        // Declarations within `static foreach` do not clash with
        // `static foreach` loop variables.
        auto forward = parent.isScopeDsymbol();
        assert(forward);
        if (!forward.symtab)
        {
            forward.symtab = new DsymbolTable();
        }
        return forward.symtabLookup(s,id);
    }

    override void importScope(Dsymbol s, Visibility visibility)
    {
        auto forward = parent.isScopeDsymbol();
        assert(forward);
        forward.importScope(s, visibility);
    }

    override const(char)* kind()const{ return "local scope"; }

}

/**
 * Class that holds an expression in a Dsymbol wrapper.
 * This is not an AST node, but a class used to pass
 * an expression as a function parameter of type Dsymbol.
 */
extern (C++) final class ExpressionDsymbol : Dsymbol
{
    Expression exp;
    this(Expression exp) nothrow @safe
    {
        super(DSYM.expressionDsymbol);
        this.exp = exp;
    }
}

/**********************************************
 * Encapsulate assigning to an alias:
 *      `identifier = type;`
 *      `identifier = symbol;`
 * where `identifier` is an AliasDeclaration in scope.
 */
extern (C++) final class AliasAssign : Dsymbol
{
    Identifier ident; /// Dsymbol's ident will be null, as this class is anonymous
    Type type;        /// replace previous RHS of AliasDeclaration with `type`
    Dsymbol aliassym; /// replace previous RHS of AliasDeclaration with `aliassym`
                      /// only one of type and aliassym can be != null

    extern (D) this(Loc loc, Identifier ident, Type type, Dsymbol aliassym) nothrow @safe
    {
        super(DSYM.aliasAssign, loc, null);
        this.ident = ident;
        this.type = type;
        this.aliassym = aliassym;
    }

    override AliasAssign syntaxCopy(Dsymbol s)
    {
        assert(!s);
        AliasAssign aa = new AliasAssign(loc, ident,
                type     ? type.syntaxCopy()         : null,
                aliassym ? aliassym.syntaxCopy(null) : null);
        return aa;
    }

    override const(char)* kind() const
    {
        return "alias assignment";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Table of Dsymbol's
 */
extern (C++) final class DsymbolTable : RootObject
{
    AssocArray!(Identifier, Dsymbol) tab;

  nothrow:

   /***************************
    * Look up Identifier in symbol table
    * Params:
    *   ident = identifer to look up
    * Returns:
    *   Dsymbol if found, null if not
    */
    Dsymbol lookup(const Identifier ident)
    {
        //printf("DsymbolTable::lookup(%s)\n", ident.toChars());
        return tab[ident];
    }

    /**********
     * Replace existing symbol in symbol table with `s`.
     * If it's not there, add it.
     * Params:
     *   s = replacement symbol with same identifier
     */
    void update(Dsymbol s)
    {
        *tab.getLvalue(s.ident) = s;
    }

    /**************************
     * Insert Dsymbol in table.
     * Params:
     *   s = symbol to add
     * Returns:
     *   null if already in table, `s` if inserted
     */
    Dsymbol insert(Dsymbol s)
    {
        return insert(s.ident, s);
    }

    /**************************
     * Insert Dsymbol in table.
     * Params:
     *   ident = identifier to serve as index
     *   s = symbol to add
     * Returns:
     *   null if already in table, `s` if inserted
     */
    Dsymbol insert(const Identifier ident, Dsymbol s)
    {
        //printf("DsymbolTable.insert(this = %p, '%s')\n", this, s.ident.toChars());
        Dsymbol* ps = tab.getLvalue(ident);
        if (*ps)
            return null; // already in table
        *ps = s;
        return s;
    }

    /*****************
     * Returns:
     *  number of symbols in symbol table
     */
    size_t length() const pure
    {
        return tab.length;
    }
}

/**
 * ImportC global `asm` definition.
 */
extern (C++) final class CAsmDeclaration : Dsymbol
{
    Expression code;
    extern (D) this(Expression e) nothrow @safe
    {
        super(DSYM.cAsmDeclaration);
        this.code = e;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
