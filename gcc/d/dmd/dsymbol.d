/**
 * The base class for a D symbol, which can be a module, variable, function, enum, etc.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dsymbol.d, _dsymbol.d)
 * Documentation:  https://dlang.org/phobos/dmd_dsymbol.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dsymbol.d
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
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.lexer;
import dmd.location;
import dmd.mtype;
import dmd.nspace;
import dmd.opover;
import dmd.root.aav;
import dmd.root.rmem;
import dmd.root.rootobject;
import dmd.root.speller;
import dmd.root.string;
import dmd.statement;
import dmd.staticassert;
import dmd.tokens;
import dmd.visitor;

/***************************************
 * Calls dg(Dsymbol *sym) for each Dsymbol.
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
 * Calls dg(Dsymbol *sym) for each Dsymbol.
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

    extern (D) this(uint old) nothrow
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

    extern (D):

    this(Visibility.Kind kind) pure nothrow @nogc @safe
    {
        this.kind = kind;
    }

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
    bool opEquals(ref const Visibility other) const
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
enum : int
{
    IgnoreNone              = 0x00, // default
    IgnorePrivateImports    = 0x01, // don't search private imports
    IgnoreErrors            = 0x02, // don't give error messages
    IgnoreAmbiguous         = 0x04, // return NULL if ambiguous
    SearchLocalsOnly        = 0x08, // only look at locals (don't search imports)
    SearchImportsOnly       = 0x10, // only look in imports
    SearchUnqualifiedModule = 0x20, // the module scope search is unqualified,
                                    // meaning don't search imports in that scope,
                                    // because qualified module searches search
                                    // their imports
    IgnoreSymbolVisibility  = 0x80, // also find private and package protected symbols
    TagNameSpace            = 0x100, // search ImportC tag symbol table
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

/***********************************************************
 */
extern (C++) class Dsymbol : ASTNode
{
    Identifier ident;
    Dsymbol parent;
    Symbol* csym;           // symbol for code generator
    const Loc loc;          // where defined
    Scope* _scope;          // !=null means context to use for semantic()
    const(char)* prettystring;  // cached value of toPrettyChars()
    private DsymbolAttributes* atts; /// attached attribute declarations
    bool errors;            // this symbol failed to pass semantic()
    PASS semanticRun = PASS.initial;
    ushort localNum;        /// perturb mangled name to avoid collisions with those in FuncDeclaration.localsymtab

    final extern (D) this() nothrow
    {
        //printf("Dsymbol::Dsymbol(%p)\n", this);
        loc = Loc(null, 0, 0);
    }

    final extern (D) this(Identifier ident) nothrow
    {
        //printf("Dsymbol::Dsymbol(%p, ident)\n", this);
        this.loc = Loc(null, 0, 0);
        this.ident = ident;
    }

    final extern (D) this(const ref Loc loc, Identifier ident) nothrow
    {
        //printf("Dsymbol::Dsymbol(%p, ident)\n", this);
        this.loc = loc;
        this.ident = ident;
    }

    static Dsymbol create(Identifier ident) nothrow
    {
        return new Dsymbol(ident);
    }

    override const(char)* toChars() const
    {
        return ident ? ident.toChars() : "__anonymous";
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

    final const(Loc) getLoc()
    {
        if (!loc.isValid()) // avoid bug 5861.
            if (const m = getModule())
                return Loc(m.srcfile.toChars(), 0, 0);
        return loc;
    }

    final const(char)* locToChars()
    {
        return getLoc().toChars();
    }

    override bool equals(const RootObject o) const
    {
        if (this == o)
            return true;
        if (o.dyncast() != DYNCAST.dsymbol)
            return false;
        auto s = cast(Dsymbol)o;
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

    static if (__VERSION__ < 2092)
    {
        final void error(const ref Loc loc, const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            .verror(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }

        final void error(const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            const loc = getLoc();
            .verror(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }

        final void deprecation(const ref Loc loc, const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            .vdeprecation(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }

        final void deprecation(const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            const loc = getLoc();
            .vdeprecation(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }
    }
    else
    {
        pragma(printf) final void error(const ref Loc loc, const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            .verror(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }

        pragma(printf) final void error(const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            const loc = getLoc();
            .verror(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }

        pragma(printf) final void deprecation(const ref Loc loc, const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            .vdeprecation(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }

        pragma(printf) final void deprecation(const(char)* format, ...)
        {
            va_list ap;
            va_start(ap, format);
            const loc = getLoc();
            .vdeprecation(loc, format, ap, kind(), prettyFormatHelper().ptr);
            va_end(ap);
        }
    }

    final bool checkDeprecated(const ref Loc loc, Scope* sc)
    {
        if (global.params.useDeprecated == DiagnosticReporting.off)
            return false;
        if (!this.isDeprecated())
            return false;
        // Don't complain if we're inside a deprecated symbol's scope
        if (sc.isDeprecated())
            return false;
        // Don't complain if we're inside a template constraint
        // https://issues.dlang.org/show_bug.cgi?id=21831
        if (sc.flags & SCOPE.constraint)
            return false;

        const(char)* message = null;
        for (Dsymbol p = this; p; p = p.parent)
        {
            message = p.depdecl ? p.depdecl.getMessage() : null;
            if (message)
                break;
        }
        if (message)
            deprecation(loc, "is deprecated - %s", message);
        else
            deprecation(loc, "is deprecated");

        if (auto ti = sc.parent ? sc.parent.isInstantiated() : null)
            ti.printInstantiationTrace(Classification.deprecation);
        else if (auto ti = sc.parent ? sc.parent.isTemplateInstance() : null)
            ti.printInstantiationTrace(Classification.deprecation);

        return true;
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
            Module m = s.isModule();
            if (m)
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
            Module m = s.isModule();
            if (m)
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
    final inout(Dsymbol) pastMixin() inout
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
    final inout(Dsymbol) toParent() inout
    {
        return parent ? parent.pastMixin() : null;
    }

    /// ditto
    final inout(Dsymbol) toParent2() inout
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
                    else if (p2 && sa == p2)
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
        uint oldgag = global.gag;
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
        if (prettystring && !QualifyTypes)
            return prettystring;

        //printf("Dsymbol::toPrettyChars() '%s'\n", toChars());
        if (!parent)
        {
            auto s = toChars();
            if (!QualifyTypes)
                prettystring = s;
            return s;
        }

        // Computer number of components
        size_t complength = 0;
        for (Dsymbol p = this; p; p = p.parent)
            ++complength;

        // Allocate temporary array comp[]
        alias T = const(char)[];
        auto compptr = cast(T*)Mem.check(malloc(complength * T.sizeof));
        auto comp = compptr[0 .. complength];

        // Fill in comp[] and compute length of final result
        size_t length = 0;
        int i;
        for (Dsymbol p = this; p; p = p.parent)
        {
            const s = QualifyTypes ? p.toPrettyCharsHelper() : p.toChars();
            const len = strlen(s);
            comp[i] = s[0 .. len];
            ++i;
            length += len + 1;
        }

        auto s = cast(char*)mem.xmalloc_noscan(length);
        auto q = s + length - 1;
        *q = 0;
        foreach (j; 0 .. complength)
        {
            const t = comp[j].ptr;
            const len = comp[j].length;
            q -= len;
            memcpy(q, t, len);
            if (q == s)
                break;
            *--q = '.';
        }
        free(comp.ptr);
        if (!QualifyTypes)
            prettystring = s;
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

    void addMember(Scope* sc, ScopeDsymbol sds)
    {
        //printf("Dsymbol::addMember('%s')\n", toChars());
        //printf("Dsymbol::addMember(this = %p, '%s' scopesym = '%s')\n", this, toChars(), sds.toChars());
        //printf("Dsymbol::addMember(this = %p, '%s' sds = %p, sds.symtab = %p)\n", this, toChars(), sds, sds.symtab);
        parent = sds;
        if (isAnonymous()) // no name, so can't add it to symbol table
            return;

        if (!sds.symtabInsert(this)) // if name is already defined
        {
            if (isAliasDeclaration() && !_scope)
                setScope(sc);
            Dsymbol s2 = sds.symtabLookup(this,ident);
            /* https://issues.dlang.org/show_bug.cgi?id=17434
             *
             * If we are trying to add an import to the symbol table
             * that has already been introduced, then keep the one with
             * larger visibility. This is fine for imports because if
             * we have multiple imports of the same file, if a single one
             * is public then the symbol is reachable.
             */
            if (auto i1 = isImport())
            {
                if (auto i2 = s2.isImport())
                {
                    if (sc.explicitVisibility && sc.visibility > i2.visibility)
                        sds.symtab.update(this);
                }
            }

            // If using C tag/prototype/forward declaration rules
            if (sc.flags & SCOPE.Cfile && !this.isImport())
            {
                if (handleTagSymbols(*sc, this, s2, sds))
                    return;
                if (handleSymbolRedeclarations(*sc, this, s2, sds))
                    return;

                sds.multiplyDefined(Loc.initial, this, s2);  // ImportC doesn't allow overloading
                errors = true;
                return;
            }

            if (!s2.overloadInsert(this))
            {
                sds.multiplyDefined(Loc.initial, this, s2);
                errors = true;
            }
        }
        if (sds.isAggregateDeclaration() || sds.isEnumDeclaration())
        {
            if (ident == Id.__sizeof ||
                !(sc && sc.flags & SCOPE.Cfile) && (ident == Id.__xalignof || ident == Id._mangleof))
            {
                error("`.%s` property cannot be redefined", ident.toChars());
                errors = true;
            }
        }
    }

    /*************************************
     * Set scope for future semantic analysis so we can
     * deal better with forward references.
     */
    void setScope(Scope* sc)
    {
        //printf("Dsymbol::setScope() %p %s, %p stc = %llx\n", this, toChars(), sc, sc.stc);
        if (!sc.nofree)
            sc.setNoFree(); // may need it even after semantic() finishes
        _scope = sc;
        if (sc.depdecl)
            depdecl = sc.depdecl;
        if (!userAttribDecl)
            userAttribDecl = sc.userAttribDecl;
    }

    void importAll(Scope* sc)
    {
    }

    /*********************************************
     * Search for ident as member of s.
     * Params:
     *  loc = location to print for error messages
     *  ident = identifier to search for
     *  flags = IgnoreXXXX
     * Returns:
     *  null if not found
     */
    Dsymbol search(const ref Loc loc, Identifier ident, int flags = IgnoreNone)
    {
        //printf("Dsymbol::search(this=%p,%s, ident='%s')\n", this, toChars(), ident.toChars());
        return null;
    }

    extern (D) final Dsymbol search_correct(Identifier ident)
    {
        /***************************************************
         * Search for symbol with correct spelling.
         */
        extern (D) Dsymbol symbol_search_fp(const(char)[] seed, out int cost)
        {
            /* If not in the lexer's string table, it certainly isn't in the symbol table.
             * Doing this first is a lot faster.
             */
            if (!seed.length)
                return null;
            Identifier id = Identifier.lookup(seed);
            if (!id)
                return null;
            cost = 0;   // all the same cost
            Dsymbol s = this;
            Module.clearCache();
            return s.search(Loc.initial, id, IgnoreErrors);
        }

        if (global.gag)
            return null; // don't do it for speculative compiles; too time consuming
        // search for exact name first
        if (auto s = search(Loc.initial, ident, IgnoreErrors))
            return s;
        return speller!symbol_search_fp(ident.toString());
    }

    /***************************************
     * Search for identifier id as a member of `this`.
     * `id` may be a template instance.
     *
     * Params:
     *  loc = location to print the error messages
     *  sc = the scope where the symbol is located
     *  id = the id of the symbol
     *  flags = the search flags which can be `SearchLocalsOnly` or `IgnorePrivateImports`
     *
     * Returns:
     *      symbol found, NULL if not
     */
    extern (D) final Dsymbol searchX(const ref Loc loc, Scope* sc, RootObject id, int flags)
    {
        //printf("Dsymbol::searchX(this=%p,%s, ident='%s')\n", this, toChars(), ident.toChars());
        Dsymbol s = toAlias();
        Dsymbol sm;
        if (Declaration d = s.isDeclaration())
        {
            if (d.inuse)
            {
                .error(loc, "circular reference to `%s`", d.toPrettyChars());
                return null;
            }
        }
        switch (id.dyncast())
        {
        case DYNCAST.identifier:
            sm = s.search(loc, cast(Identifier)id, flags);
            break;
        case DYNCAST.dsymbol:
            {
                // It's a template instance
                //printf("\ttemplate instance id\n");
                Dsymbol st = cast(Dsymbol)id;
                TemplateInstance ti = st.isTemplateInstance();
                sm = s.search(loc, ti.name);
                if (!sm)
                    return null;
                sm = sm.toAlias();
                TemplateDeclaration td = sm.isTemplateDeclaration();
                if (!td)
                    return null; // error but handled later
                ti.tempdecl = td;
                if (!ti.semanticRun)
                    ti.dsymbolSemantic(sc);
                sm = ti.toAlias();
                break;
            }
        case DYNCAST.type:
        case DYNCAST.expression:
        default:
            assert(0);
        }
        return sm;
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
    uinteger_t size(const ref Loc loc)
    {
        error("symbol `%s` has no size", toChars());
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

    /**************************************
     * Determine if this symbol is only one.
     * Returns:
     *      false, *ps = NULL: There are 2 or more symbols
     *      true,  *ps = NULL: There are zero symbols
     *      true,  *ps = symbol: The one and only one symbol
     */
    bool oneMember(Dsymbol* ps, Identifier ident)
    {
        //printf("Dsymbol::oneMember()\n");
        *ps = this;
        return true;
    }

    /*****************************************
     * Same as Dsymbol::oneMember(), but look at an array of Dsymbols.
     */
    extern (D) static bool oneMembers(Dsymbols* members, Dsymbol* ps, Identifier ident)
    {
        //printf("Dsymbol::oneMembers() %d\n", members ? members.length : 0);
        Dsymbol s = null;
        if (!members)
        {
            *ps = null;
            return true;
        }

        for (size_t i = 0; i < members.length; i++)
        {
            Dsymbol sx = (*members)[i];
            bool x = sx.oneMember(ps, ident);
            //printf("\t[%d] kind %s = %d, s = %p\n", i, sx.kind(), x, *ps);
            if (!x)
            {
                //printf("\tfalse 1\n");
                assert(*ps is null);
                return false;
            }
            if (*ps)
            {
                assert(ident);
                if (!(*ps).ident || !(*ps).ident.equals(ident))
                    continue;
                if (!s)
                    s = *ps;
                else if (s.isOverloadable() && (*ps).isOverloadable())
                {
                    // keep head of overload set
                    FuncDeclaration f1 = s.isFuncDeclaration();
                    FuncDeclaration f2 = (*ps).isFuncDeclaration();
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
                    *ps = null;
                    //printf("\tfalse 2\n");
                    return false;
                }
            }
        }
        *ps = s; // s is the one symbol, null if none
        //printf("\ttrue\n");
        return true;
    }

    void setFieldOffset(AggregateDeclaration ad, ref FieldState fieldState, bool isunion)
    {
    }

    /*****************************************
     * Is Dsymbol a variable that contains pointers?
     */
    bool hasPointers()
    {
        //printf("Dsymbol::hasPointers() %s\n", toChars());
        return false;
    }

    bool hasStaticCtorOrDtor()
    {
        //printf("Dsymbol::hasStaticCtorOrDtor() %s\n", toChars());
        return false;
    }

    void addObjcSymbols(ClassDeclarations* classes, ClassDeclarations* categories)
    {
    }

    void checkCtorConstInit()
    {
    }

    /****************************************
     * Add documentation comment to Dsymbol.
     * Ignore NULL comments.
     */
    void addComment(const(char)* comment)
    {
        if (!comment || !*comment)
            return;

        //printf("addComment '%s' to Dsymbol %p '%s'\n", comment, this, toChars());
        void* h = cast(void*)this;      // just the pointer is the key
        auto p = h in commentHashTable;
        if (!p)
        {
            commentHashTable[h] = comment;
            return;
        }
        if (strcmp(*p, comment) != 0)
        {
            // Concatenate the two
            *p = Lexer.combineComments((*p).toDString(), comment.toDString(), true);
        }
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

    private extern (D) __gshared const(char)*[void*] commentHashTable;


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

  pure nothrow @safe @nogc:

    // Eliminate need for dynamic_cast
    inout(Package)                     isPackage()                     inout { return null; }
    inout(Module)                      isModule()                      inout { return null; }
    inout(EnumMember)                  isEnumMember()                  inout { return null; }
    inout(TemplateDeclaration)         isTemplateDeclaration()         inout { return null; }
    inout(TemplateInstance)            isTemplateInstance()            inout { return null; }
    inout(TemplateMixin)               isTemplateMixin()               inout { return null; }
    inout(ForwardingAttribDeclaration) isForwardingAttribDeclaration() inout { return null; }
    inout(Nspace)                      isNspace()                      inout { return null; }
    inout(Declaration)                 isDeclaration()                 inout { return null; }
    inout(StorageClassDeclaration)     isStorageClassDeclaration()     inout { return null; }
    inout(ExpressionDsymbol)           isExpressionDsymbol()           inout { return null; }
    inout(AliasAssign)                 isAliasAssign()                 inout { return null; }
    inout(ThisDeclaration)             isThisDeclaration()             inout { return null; }
    inout(BitFieldDeclaration)         isBitFieldDeclaration()         inout { return null; }
    inout(TypeInfoDeclaration)         isTypeInfoDeclaration()         inout { return null; }
    inout(TupleDeclaration)            isTupleDeclaration()            inout { return null; }
    inout(AliasDeclaration)            isAliasDeclaration()            inout { return null; }
    inout(AggregateDeclaration)        isAggregateDeclaration()        inout { return null; }
    inout(FuncDeclaration)             isFuncDeclaration()             inout { return null; }
    inout(FuncAliasDeclaration)        isFuncAliasDeclaration()        inout { return null; }
    inout(OverDeclaration)             isOverDeclaration()             inout { return null; }
    inout(FuncLiteralDeclaration)      isFuncLiteralDeclaration()      inout { return null; }
    inout(CtorDeclaration)             isCtorDeclaration()             inout { return null; }
    inout(PostBlitDeclaration)         isPostBlitDeclaration()         inout { return null; }
    inout(DtorDeclaration)             isDtorDeclaration()             inout { return null; }
    inout(StaticCtorDeclaration)       isStaticCtorDeclaration()       inout { return null; }
    inout(StaticDtorDeclaration)       isStaticDtorDeclaration()       inout { return null; }
    inout(SharedStaticCtorDeclaration) isSharedStaticCtorDeclaration() inout { return null; }
    inout(SharedStaticDtorDeclaration) isSharedStaticDtorDeclaration() inout { return null; }
    inout(InvariantDeclaration)        isInvariantDeclaration()        inout { return null; }
    inout(UnitTestDeclaration)         isUnitTestDeclaration()         inout { return null; }
    inout(NewDeclaration)              isNewDeclaration()              inout { return null; }
    inout(VarDeclaration)              isVarDeclaration()              inout { return null; }
    inout(VersionSymbol)               isVersionSymbol()               inout { return null; }
    inout(DebugSymbol)                 isDebugSymbol()                 inout { return null; }
    inout(ClassDeclaration)            isClassDeclaration()            inout { return null; }
    inout(StructDeclaration)           isStructDeclaration()           inout { return null; }
    inout(UnionDeclaration)            isUnionDeclaration()            inout { return null; }
    inout(InterfaceDeclaration)        isInterfaceDeclaration()        inout { return null; }
    inout(ScopeDsymbol)                isScopeDsymbol()                inout { return null; }
    inout(ForwardingScopeDsymbol)      isForwardingScopeDsymbol()      inout { return null; }
    inout(WithScopeSymbol)             isWithScopeSymbol()             inout { return null; }
    inout(ArrayScopeSymbol)            isArrayScopeSymbol()            inout { return null; }
    inout(Import)                      isImport()                      inout { return null; }
    inout(EnumDeclaration)             isEnumDeclaration()             inout { return null; }
    inout(SymbolDeclaration)           isSymbolDeclaration()           inout { return null; }
    inout(AttribDeclaration)           isAttribDeclaration()           inout { return null; }
    inout(AnonDeclaration)             isAnonDeclaration()             inout { return null; }
    inout(CPPNamespaceDeclaration)     isCPPNamespaceDeclaration()     inout { return null; }
    inout(VisibilityDeclaration)       isVisibilityDeclaration()       inout { return null; }
    inout(OverloadSet)                 isOverloadSet()                 inout { return null; }
    inout(CompileDeclaration)          isCompileDeclaration()          inout { return null; }
    inout(StaticAssert)                isStaticAssert()                inout { return null; }
    inout(StaticIfDeclaration)         isStaticIfDeclaration()         inout { return null; }
}

/***********************************************************
 * Dsymbol that generates a scope
 */
extern (C++) class ScopeDsymbol : Dsymbol
{
    Dsymbols* members;          // all Dsymbol's in this scope
    DsymbolTable symtab;        // members[] sorted into table
    uint endlinnum;             // the linnumber of the statement after the scope (0 if unknown)

private:
    /// symbols whose members have been imported, i.e. imported modules and template mixins
    Dsymbols* importedScopes;
    Visibility.Kind* visibilities; // array of Visibility.Kind, one for each import

    import dmd.root.bitarray;
    BitArray accessiblePackages, privateAccessiblePackages;// whitelists of accessible (imported) packages

public:
    final extern (D) this() nothrow
    {
    }

    final extern (D) this(Identifier ident) nothrow
    {
        super(ident);
    }

    final extern (D) this(const ref Loc loc, Identifier ident) nothrow
    {
        super(loc, ident);
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

    /*****************************************
     * This function is #1 on the list of functions that eat cpu time.
     * Be very, very careful about slowing it down.
     */
    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        //printf("%s.ScopeDsymbol::search(ident='%s', flags=x%x)\n", toChars(), ident.toChars(), flags);
        //if (strcmp(ident.toChars(),"c") == 0) *(char*)0=0;

        // Look in symbols declared in this module
        if (symtab && !(flags & SearchImportsOnly))
        {
            //printf(" look in locals\n");
            auto s1 = symtab.lookup(ident);
            if (s1)
            {
                //printf("\tfound in locals = '%s.%s'\n",toChars(),s1.toChars());
                return s1;
            }
        }
        //printf(" not found in locals\n");

        // Look in imported scopes
        if (!importedScopes)
            return null;

        //printf(" look in imports\n");
        Dsymbol s = null;
        OverloadSet a = null;
        // Look in imported modules
        for (size_t i = 0; i < importedScopes.length; i++)
        {
            // If private import, don't search it
            if ((flags & IgnorePrivateImports) && visibilities[i] == Visibility.Kind.private_)
                continue;
            int sflags = flags & (IgnoreErrors | IgnoreAmbiguous); // remember these in recursive searches
            Dsymbol ss = (*importedScopes)[i];
            //printf("\tscanning import '%s', visibilities = %d, isModule = %p, isImport = %p\n", ss.toChars(), visibilities[i], ss.isModule(), ss.isImport());

            if (ss.isModule())
            {
                if (flags & SearchLocalsOnly)
                    continue;
            }
            else // mixin template
            {
                if (flags & SearchImportsOnly)
                    continue;

                sflags |= SearchLocalsOnly;
            }

            /* Don't find private members if ss is a module
             */
            Dsymbol s2 = ss.search(loc, ident, sflags | (ss.isModule() ? IgnorePrivateImports : IgnoreNone));
            import dmd.access : symbolIsVisible;
            if (!s2 || !(flags & IgnoreSymbolVisibility) && !symbolIsVisible(this, s2))
                continue;
            if (!s)
            {
                s = s2;
                if (s && s.isOverloadSet())
                    a = mergeOverloadSet(ident, a, s);
            }
            else if (s2 && s != s2)
            {
                if (s.toAlias() == s2.toAlias() || s.getType() == s2.getType() && s.getType())
                {
                    /* After following aliases, we found the same
                     * symbol, so it's not an ambiguity.  But if one
                     * alias is deprecated or less accessible, prefer
                     * the other.
                     */
                    if (s.isDeprecated() || s.visible() < s2.visible() && s2.visible().kind != Visibility.Kind.none)
                        s = s2;
                }
                else
                {
                    /* Two imports of the same module should be regarded as
                     * the same.
                     */
                    Import i1 = s.isImport();
                    Import i2 = s2.isImport();
                    if (!(i1 && i2 && (i1.mod == i2.mod || (!i1.parent.isImport() && !i2.parent.isImport() && i1.ident.equals(i2.ident)))))
                    {
                        /* https://issues.dlang.org/show_bug.cgi?id=8668
                         * Public selective import adds AliasDeclaration in module.
                         * To make an overload set, resolve aliases in here and
                         * get actual overload roots which accessible via s and s2.
                         */
                        s = s.toAlias();
                        s2 = s2.toAlias();
                        /* If both s2 and s are overloadable (though we only
                         * need to check s once)
                         */

                        auto so2 = s2.isOverloadSet();
                        if ((so2 || s2.isOverloadable()) && (a || s.isOverloadable()))
                        {
                            if (symbolIsVisible(this, s2))
                            {
                                a = mergeOverloadSet(ident, a, s2);
                            }
                            if (!symbolIsVisible(this, s))
                                s = s2;
                            continue;
                        }

                        /* Two different overflow sets can have the same members
                         * https://issues.dlang.org/show_bug.cgi?id=16709
                         */
                        auto so = s.isOverloadSet();
                        if (so && so2)
                        {
                            if (so.a.length == so2.a.length)
                            {
                                foreach (j; 0 .. so.a.length)
                                {
                                    if (so.a[j] !is so2.a[j])
                                        goto L1;
                                }
                                continue;  // the same
                              L1:
                                {   } // different
                            }
                        }

                        if (flags & IgnoreAmbiguous) // if return NULL on ambiguity
                            return null;

                        /* If two imports from C import files, pick first one, as C has global name space
                         */
                        if (s.isCsymbol() && s2.isCsymbol())
                            continue;

                        if (!(flags & IgnoreErrors))
                            ScopeDsymbol.multiplyDefined(loc, s, s2);
                        break;
                    }
                }
            }
        }
        if (s)
        {
            /* Build special symbol if we had multiple finds
             */
            if (a)
            {
                if (!s.isOverloadSet())
                    a = mergeOverloadSet(ident, a, s);
                s = a;
            }
            //printf("\tfound in imports %s.%s\n", toChars(), s.toChars());
            return s;
        }
        //printf(" not found in imports\n");
        return null;
    }

    extern (D) private OverloadSet mergeOverloadSet(Identifier ident, OverloadSet os, Dsymbol s)
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

    bool isPackageAccessible(Package p, Visibility visibility, int flags = 0) nothrow
    {
        if (p.tag < accessiblePackages.length && accessiblePackages[p.tag] ||
            visibility.kind == Visibility.Kind.private_ && p.tag < privateAccessiblePackages.length && privateAccessiblePackages[p.tag])
            return true;
        foreach (i, ss; importedScopes ? (*importedScopes)[] : null)
        {
            // only search visible scopes && imported modules should ignore private imports
            if (visibility.kind <= visibilities[i] &&
                ss.isScopeDsymbol.isPackageAccessible(p, visibility, IgnorePrivateImports))
                return true;
        }
        return false;
    }

    override final bool isforwardRef() nothrow
    {
        return (members is null);
    }

    static void multiplyDefined(const ref Loc loc, Dsymbol s1, Dsymbol s2)
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
            s1.error(s1.loc, "conflicts with %s `%s` at %s", s2.kind(), s2.toPrettyChars(), s2.locToChars());
        }
    }

    override const(char)* kind() const
    {
        return "ScopeDsymbol";
    }

    /*******************************************
     * Look for member of the form:
     *      const(MemberInfo)[] getMembers(string);
     * Returns NULL if not found
     */
    final FuncDeclaration findGetMembers()
    {
        Dsymbol s = search_function(this, Id.getmembers);
        FuncDeclaration fdx = s ? s.isFuncDeclaration() : null;
        version (none)
        {
            // Finish
            __gshared TypeFunction tfgetmembers;
            if (!tfgetmembers)
            {
                Scope sc;
                auto parameters = new Parameters();
                Parameters* p = new Parameter(STC.in_, Type.tchar.constOf().arrayOf(), null, null);
                parameters.push(p);
                Type tret = null;
                tfgetmembers = new TypeFunction(parameters, tret, VarArg.none, LINK.d);
                tfgetmembers = cast(TypeFunction)tfgetmembers.dsymbolSemantic(Loc.initial, &sc);
            }
            if (fdx)
                fdx = fdx.overloadExactMatch(tfgetmembers);
        }
        if (fdx && fdx.isVirtual())
            fdx = null;
        return fdx;
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

    /****************************************
     * Return true if any of the members are static ctors or static dtors, or if
     * any members have members that are.
     */
    override bool hasStaticCtorOrDtor()
    {
        if (members)
        {
            for (size_t i = 0; i < members.length; i++)
            {
                Dsymbol member = (*members)[i];
                if (member.hasStaticCtorOrDtor())
                    return true;
            }
        }
        return false;
    }

    extern (D) alias ForeachDg = int delegate(size_t idx, Dsymbol s);

    /***************************************
     * Expands attribute declarations in members in depth first
     * order. Calls dg(size_t symidx, Dsymbol *sym) for each
     * member.
     * If dg returns !=0, stops and returns that value else returns 0.
     * Use this function to avoid the O(N + N^2/2) complexity of
     * calculating dim and calling N times getNth.
     * Returns:
     *  last value returned by dg()
     */
    extern (D) static int _foreach(Scope* sc, Dsymbols* members, scope ForeachDg dg, size_t* pn = null)
    {
        assert(dg);
        if (!members)
            return 0;
        size_t n = pn ? *pn : 0; // take over index
        int result = 0;
        foreach (size_t i; 0 .. members.length)
        {
            Dsymbol s = (*members)[i];
            if (AttribDeclaration a = s.isAttribDeclaration())
                result = _foreach(sc, a.include(sc), dg, &n);
            else if (TemplateMixin tm = s.isTemplateMixin())
                result = _foreach(sc, tm.members, dg, &n);
            else if (s.isTemplateInstance())
            {
            }
            else if (s.isUnitTestDeclaration())
            {
            }
            else
                result = dg(n++, s);
            if (result)
                break;
        }
        if (pn)
            *pn = n; // update index
        return result;
    }

    override final inout(ScopeDsymbol) isScopeDsymbol() inout
    {
        return this;
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

    extern (D) this(WithStatement withstate) nothrow
    {
        this.withstate = withstate;
    }

    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        //printf("WithScopeSymbol.search(%s)\n", ident.toChars());
        if (flags & SearchImportsOnly)
            return null;
        // Acts as proxy to the with class declaration
        Dsymbol s = null;
        Expression eold = null;
        for (Expression e = withstate.exp; e && e != eold; e = resolveAliasThis(_scope, e, true))
        {
            if (e.op == EXP.scope_)
            {
                s = (cast(ScopeExp)e).sds;
            }
            else if (e.op == EXP.type)
            {
                s = e.type.toDsymbol(null);
            }
            else
            {
                Type t = e.type.toBasetype();
                s = t.toDsymbol(null);
            }
            if (s)
            {
                s = s.search(loc, ident, flags);
                if (s)
                    return s;
            }
            eold = e;
        }
        return null;
    }

    override inout(WithScopeSymbol) isWithScopeSymbol() inout
    {
        return this;
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
    private RootObject arrayContent;
    Scope* sc;

    extern (D) this(Scope* sc, Expression exp) nothrow
    {
        super(exp.loc, null);
        assert(exp.op == EXP.index || exp.op == EXP.slice || exp.op == EXP.array);
        this.sc = sc;
        this.arrayContent = exp;
    }

    extern (D) this(Scope* sc, TypeTuple type) nothrow
    {
        this.sc = sc;
        this.arrayContent = type;
    }

    extern (D) this(Scope* sc, TupleDeclaration td) nothrow
    {
        this.sc = sc;
        this.arrayContent = td;
    }

    /// This override is used to solve `$`
    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = IgnoreNone)
    {
        //printf("ArrayScopeSymbol::search('%s', flags = %d)\n", ident.toChars(), flags);
        if (ident != Id.dollar)
            return null;

        VarDeclaration* pvar;
        Expression ce;

        static Dsymbol dollarFromTypeTuple(const ref Loc loc, TypeTuple tt, Scope* sc)
        {

            /* $ gives the number of type entries in the type tuple
             */
            auto v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, null);
            Expression e = new IntegerExp(Loc.initial, tt.arguments.length, Type.tsize_t);
            v._init = new ExpInitializer(Loc.initial, e);
            v.storage_class |= STC.temp | STC.static_ | STC.const_;
            v.dsymbolSemantic(sc);
            return v;
        }

        const DYNCAST kind = arrayContent.dyncast();
        switch (kind) with (DYNCAST)
        {
        case dsymbol:
            TupleDeclaration td = cast(TupleDeclaration) arrayContent;
            /* $ gives the number of elements in the tuple
             */
            auto v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, null);
            Expression e = new IntegerExp(Loc.initial, td.objects.length, Type.tsize_t);
            v._init = new ExpInitializer(Loc.initial, e);
            v.storage_class |= STC.temp | STC.static_ | STC.const_;
            v.dsymbolSemantic(sc);
            return v;
        case type:
            return dollarFromTypeTuple(loc, cast(TypeTuple) arrayContent, sc);
        default:
            break;
        }
        Expression exp = cast(Expression) arrayContent;
        if (auto ie = exp.isIndexExp())
        {
            /* array[index] where index is some function of $
             */
            pvar = &ie.lengthVar;
            ce = ie.e1;
        }
        else if (auto se = exp.isSliceExp())
        {
            /* array[lwr .. upr] where lwr or upr is some function of $
             */
            pvar = &se.lengthVar;
            ce = se.e1;
        }
        else if (auto ae = exp.isArrayExp())
        {
            /* array[e0, e1, e2, e3] where e0, e1, e2 are some function of $
             * $ is a opDollar!(dim)() where dim is the dimension(0,1,2,...)
             */
            pvar = &ae.lengthVar;
            ce = ae.e1;
        }
        else
        {
            /* Didn't find $, look in enclosing scope(s).
             */
            return null;
        }
        ce = ce.lastComma();
        /* If we are indexing into an array that is really a type
         * tuple, rewrite this as an index into a type tuple and
         * try again.
         */
        if (auto te = ce.isTypeExp())
        {
            if (auto ttp = te.type.isTypeTuple())
                return dollarFromTypeTuple(loc, ttp, sc);
        }
        /* *pvar is lazily initialized, so if we refer to $
         * multiple times, it gets set only once.
         */
        if (!*pvar) // if not already initialized
        {
            /* Create variable v and set it to the value of $
             */
            VarDeclaration v;
            Type t;
            if (auto tupexp = ce.isTupleExp())
            {
                /* It is for an expression tuple, so the
                 * length will be a const.
                 */
                Expression e = new IntegerExp(Loc.initial, tupexp.exps.length, Type.tsize_t);
                v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, new ExpInitializer(Loc.initial, e));
                v.storage_class |= STC.temp | STC.static_ | STC.const_;
            }
            else if (ce.type && (t = ce.type.toBasetype()) !is null && (t.ty == Tstruct || t.ty == Tclass))
            {
                // Look for opDollar
                assert(exp.op == EXP.array || exp.op == EXP.slice);
                AggregateDeclaration ad = isAggregate(t);
                assert(ad);
                Dsymbol s = ad.search(loc, Id.opDollar);
                if (!s) // no dollar exists -- search in higher scope
                    return null;
                s = s.toAlias();
                Expression e = null;
                // Check for multi-dimensional opDollar(dim) template.
                if (TemplateDeclaration td = s.isTemplateDeclaration())
                {
                    dinteger_t dim = 0;
                    if (exp.op == EXP.array)
                    {
                        dim = (cast(ArrayExp)exp).currentDimension;
                    }
                    else if (exp.op == EXP.slice)
                    {
                        dim = 0; // slices are currently always one-dimensional
                    }
                    else
                    {
                        assert(0);
                    }
                    auto tiargs = new Objects();
                    Expression edim = new IntegerExp(Loc.initial, dim, Type.tsize_t);
                    edim = edim.expressionSemantic(sc);
                    tiargs.push(edim);
                    e = new DotTemplateInstanceExp(loc, ce, td.ident, tiargs);
                }
                else
                {
                    /* opDollar exists, but it's not a template.
                     * This is acceptable ONLY for single-dimension indexing.
                     * Note that it's impossible to have both template & function opDollar,
                     * because both take no arguments.
                     */
                    if (exp.op == EXP.array && (cast(ArrayExp)exp).arguments.length != 1)
                    {
                        exp.error("`%s` only defines opDollar for one dimension", ad.toChars());
                        return null;
                    }
                    Declaration d = s.isDeclaration();
                    assert(d);
                    e = new DotVarExp(loc, ce, d);
                }
                e = e.expressionSemantic(sc);
                if (!e.type)
                    exp.error("`%s` has no value", e.toChars());
                t = e.type.toBasetype();
                if (t && t.ty == Tfunction)
                    e = new CallExp(e.loc, e);
                v = new VarDeclaration(loc, null, Id.dollar, new ExpInitializer(Loc.initial, e));
                v.storage_class |= STC.temp | STC.ctfe | STC.rvalue;
            }
            else
            {
                /* For arrays, $ will either be a compile-time constant
                 * (in which case its value in set during constant-folding),
                 * or a variable (in which case an expression is created in
                 * toir.c).
                 */

                // https://issues.dlang.org/show_bug.cgi?id=16213
                // For static arrays $ is known at compile time,
                // so declare it as a manifest constant.
                auto tsa = ce.type ? ce.type.isTypeSArray() : null;
                if (tsa)
                {
                    auto e = new ExpInitializer(loc, tsa.dim);
                    v = new VarDeclaration(loc, tsa.dim.type, Id.dollar, e, STC.manifest);
                }
                else
                {
                    auto e = new VoidInitializer(Loc.initial);
                    e.type = Type.tsize_t;
                    v = new VarDeclaration(loc, Type.tsize_t, Id.dollar, e);
                    v.storage_class |= STC.temp | STC.ctfe; // it's never a true static variable
                }
            }
            *pvar = v;
        }
        (*pvar).dsymbolSemantic(sc);
        return (*pvar);
    }

    override inout(ArrayScopeSymbol) isArrayScopeSymbol() inout
    {
        return this;
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
        super(ident);
        if (os)
        {
            a.pushSlice(os.a[]);
        }
    }

    void push(Dsymbol s) nothrow
    {
        a.push(s);
    }

    override inout(OverloadSet) isOverloadSet() inout
    {
        return this;
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
    extern (D) this() nothrow
    {
        super();
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

    override inout(ForwardingScopeDsymbol) isForwardingScopeDsymbol() inout nothrow
    {
        return this;
    }

}

/**
 * Class that holds an expression in a Dsymbol wrapper.
 * This is not an AST node, but a class used to pass
 * an expression as a function parameter of type Dsymbol.
 */
extern (C++) final class ExpressionDsymbol : Dsymbol
{
    Expression exp;
    this(Expression exp) nothrow
    {
        super();
        this.exp = exp;
    }

    override inout(ExpressionDsymbol) isExpressionDsymbol() inout nothrow
    {
        return this;
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

    extern (D) this(const ref Loc loc, Identifier ident, Type type, Dsymbol aliassym) nothrow
    {
        super(loc, null);
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

    override inout(AliasAssign) isAliasAssign() inout
    {
        return this;
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

/**********************************************
 * ImportC tag symbols sit in a parallel symbol table,
 * so that this C code works:
 * ---
 * struct S { a; };
 * int S;
 * struct S s;
 * ---
 * But there are relatively few such tag symbols, so that would be
 * a waste of memory and complexity. An additional problem is we'd like the D side
 * to find the tag symbols with ordinary lookup, not lookup in both
 * tables, if the tag symbol is not conflicting with an ordinary symbol.
 * The solution is to put the tag symbols that conflict into an associative
 * array, indexed by the address of the ordinary symbol that conflicts with it.
 * C has no modules, so this associative array is tagSymTab[] in ModuleDeclaration.
 * A side effect of our approach is that D code cannot access a tag symbol that is
 * hidden by an ordinary symbol. This is more of a theoretical problem, as nobody
 * has mentioned it when importing C headers. If someone wants to do it,
 * too bad so sad. Change the C code.
 * This function fixes up the symbol table when faced with adding a new symbol
 * `s` when there is an existing symbol `s2` with the same name.
 * C also allows forward and prototype declarations of tag symbols,
 * this function merges those.
 * Params:
 *      sc = context
 *      s = symbol to add to symbol table
 *      s2 = existing declaration
 *      sds = symbol table
 * Returns:
 *      if s and s2 are successfully put in symbol table then return the merged symbol,
 *      null if they conflict
 */
Dsymbol handleTagSymbols(ref Scope sc, Dsymbol s, Dsymbol s2, ScopeDsymbol sds)
{
    enum log = false;
    if (log) printf("handleTagSymbols('%s') add %p existing %p\n", s.toChars(), s, s2);
    if (log) printf("  add %s %s, existing %s %s\n", s.kind(), s.toChars(), s2.kind(), s2.toChars());
    auto sd = s.isScopeDsymbol(); // new declaration
    auto sd2 = s2.isScopeDsymbol(); // existing declaration

    static if (log) void print(EnumDeclaration sd)
    {
        printf("members: %p\n", sd.members);
        printf("symtab: %p\n", sd.symtab);
        printf("endlinnum: %d\n", sd.endlinnum);
        printf("type: %s\n", sd.type.toChars());
        printf("memtype: %s\n", sd.memtype.toChars());
    }

    if (!sd2)
    {
        /* Look in tag table
         */
        if (log) printf(" look in tag table\n");
        if (auto p = cast(void*)s2 in sc._module.tagSymTab)
        {
            Dsymbol s2tag = *p;
            sd2 = s2tag.isScopeDsymbol();
            assert(sd2);        // only tags allowed in tag symbol table
        }
    }

    if (sd && sd2) // `s` is a tag, `sd2` is the same tag
    {
        if (log) printf(" tag is already defined\n");

        if (sd.kind() != sd2.kind())  // being enum/struct/union must match
            return null;              // conflict

        /* Not a redeclaration if one is a forward declaration.
         * Move members to the first declared type, which is sd2.
         */
        if (sd2.members)
        {
            if (!sd.members)
                return sd2;  // ignore the sd redeclaration
        }
        else if (sd.members)
        {
            sd2.members = sd.members; // transfer definition to sd2
            sd.members = null;
            if (auto ed2 = sd2.isEnumDeclaration())
            {
                auto ed = sd.isEnumDeclaration();
                if (ed.memtype != ed2.memtype)
                    return null;        // conflict

                // transfer ed's members to sd2
                ed2.members.foreachDsymbol( (s)
                {
                    if (auto em = s.isEnumMember())
                        em.ed = ed2;
                });

                ed2.type = ed.type;
                ed2.memtype = ed.memtype;
                ed2.added = false;
            }
            return sd2;
        }
        else
            return sd2; // ignore redeclaration
    }
    else if (sd) // `s` is a tag, `s2` is not
    {
        if (log) printf(" s is tag, s2 is not\n");
        /* add `s` as tag indexed by s2
         */
        sc._module.tagSymTab[cast(void*)s2] = s;
        return s;
    }
    else if (s2 is sd2) // `s2` is a tag, `s` is not
    {
        if (log) printf(" s2 is tag, s is not\n");
        /* replace `s2` in symbol table with `s`,
         * then add `s2` as tag indexed by `s`
         */
        sds.symtab.update(s);
        sc._module.tagSymTab[cast(void*)s] = s2;
        return s;
    }
    // neither s2 nor s is a tag
    if (log) printf(" collision\n");
    return null;
}


/**********************************************
 * ImportC allows redeclarations of C variables, functions and typedefs.
 *    extern int x;
 *    int x = 3;
 * and:
 *    extern void f();
 *    void f() { }
 * Attempt to merge them.
 * Params:
 *      sc = context
 *      s = symbol to add to symbol table
 *      s2 = existing declaration
 *      sds = symbol table
 * Returns:
 *      if s and s2 are successfully put in symbol table then return the merged symbol,
 *      null if they conflict
 */
Dsymbol handleSymbolRedeclarations(ref Scope sc, Dsymbol s, Dsymbol s2, ScopeDsymbol sds)
{
    enum log = false;
    if (log) printf("handleSymbolRedeclarations('%s')\n", s.toChars());
    if (log) printf("  add %s %s, existing %s %s\n", s.kind(), s.toChars(), s2.kind(), s2.toChars());

    static Dsymbol collision()
    {
        if (log) printf(" collision\n");
        return null;
    }
    /*
    Handle merging declarations with asm("foo") and their definitions
    */
    static void mangleWrangle(Declaration oldDecl, Declaration newDecl)
    {
        if (oldDecl && newDecl)
        {
            newDecl.mangleOverride = oldDecl.mangleOverride ? oldDecl.mangleOverride : null;
        }
    }

    auto vd = s.isVarDeclaration(); // new declaration
    auto vd2 = s2.isVarDeclaration(); // existing declaration

    if (vd && vd.isCmacro())
        return vd2;

    assert(!(vd2 && vd2.isCmacro()));

    if (vd && vd2)
    {
        /* if one is `static` and the other isn't, the result is undefined
         * behavior, C11 6.2.2.7
         */
        if ((vd.storage_class ^ vd2.storage_class) & STC.static_)
            return collision();

        const i1 =  vd._init && ! vd._init.isVoidInitializer();
        const i2 = vd2._init && !vd2._init.isVoidInitializer();

        if (i1 && i2)
            return collision();         // can't both have initializers

        mangleWrangle(vd2, vd);

        if (i1)                         // vd is the definition
        {
            vd2.storage_class |= STC.extern_;  // so toObjFile() won't emit it
            sds.symtab.update(vd);      // replace vd2 with the definition
            return vd;
        }

        /* BUG: the types should match, which needs semantic() to be run on it
         *    extern int x;
         *    int x;  // match
         *    typedef int INT;
         *    INT x;  // match
         *    long x; // collision
         * We incorrectly ignore these collisions
         */
        return vd2;
    }

    auto fd = s.isFuncDeclaration(); // new declaration
    auto fd2 = s2.isFuncDeclaration(); // existing declaration
    if (fd && fd2)
    {
        /* if one is `static` and the other isn't, the result is undefined
         * behavior, C11 6.2.2.7
         * However, match what gcc allows:
         *    static int sun1(); int sun1() { return 0; }
         * and:
         *    static int sun2() { return 0; } int sun2();
         * Both produce a static function.
         *
         * Both of these should fail:
         *    int sun3(); static int sun3() { return 0; }
         * and:
         *    int sun4() { return 0; } static int sun4();
         */
        // if adding `static`
        if (   fd.storage_class & STC.static_ &&
            !(fd2.storage_class & STC.static_))
        {
            return collision();
        }

        if (fd.fbody && fd2.fbody)
            return collision();         // can't both have bodies

        mangleWrangle(fd2, fd);

        if (fd.fbody)                   // fd is the definition
        {
            if (log) printf(" replace existing with new\n");
            sds.symtab.update(fd);      // replace fd2 in symbol table with fd
            fd.overnext = fd2;

            /* If fd2 is covering a tag symbol, then fd has to cover the same one
             */
            auto ps = cast(void*)fd2 in sc._module.tagSymTab;
            if (ps)
                sc._module.tagSymTab[cast(void*)fd] = *ps;

            return fd;
        }

        /* Just like with VarDeclaration, the types should match, which needs semantic() to be run on it.
         * FuncDeclaration::semantic() detects this, but it relies on .overnext being set.
         */
        fd2.overloadInsert(fd);

        return fd2;
    }

    auto td  = s.isAliasDeclaration();  // new declaration
    auto td2 = s2.isAliasDeclaration(); // existing declaration
    if (td && td2)
    {
        /* BUG: just like with variables and functions, the types should match, which needs semantic() to be run on it.
         * FuncDeclaration::semantic2() can detect this, but it relies overnext being set.
         */
        return td2;
    }

    return collision();
}
