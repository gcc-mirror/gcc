/**
 * Defines declarations of various attributes.
 *
 * The term 'attribute' refers to things that can apply to a larger scope than a single declaration.
 * Among them are:
 * - Alignment (`align(8)`)
 * - User defined attributes (`@UDA`)
 * - Function Attributes (`@safe`)
 * - Storage classes (`static`, `__gshared`)
 * - Mixin declarations  (`mixin("int x;")`)
 * - Conditional compilation (`static if`, `static foreach`)
 * - Linkage (`extern(C)`)
 * - Anonymous structs / unions
 * - Protection (`private`, `public`)
 * - Deprecated declarations (`@deprecated`)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/attrib.d, _attrib.d)
 * Documentation:  https://dlang.org/phobos/dmd_attrib.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/attrib.d
 */

module dmd.attrib;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.cond;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem : setScope, addMember, include;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.hdrgen : visibilityToBuffer;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.objc; // for objc.addSymbols
import dmd.common.outbuffer;
import dmd.root.array; // for each
import dmd.visitor;

/***********************************************************
 * Abstract attribute applied to Dsymbol's used as a common
 * ancestor for storage classes (StorageClassDeclaration),
 * linkage (LinkageDeclaration) and others.
 */
extern (C++) abstract class AttribDeclaration : Dsymbol
{
    Dsymbols* decl;     /// Dsymbol's affected by this AttribDeclaration

    extern (D) this(Dsymbols* decl) @safe
    {
        this.decl = decl;
    }

    extern (D) this(const ref Loc loc, Dsymbols* decl) @safe
    {
        super(loc, null);
        this.decl = decl;
    }

    extern (D) this(const ref Loc loc, Identifier ident, Dsymbols* decl) @safe
    {
        super(loc, ident);
        this.decl = decl;
    }

    /****************************************
     * Create a new scope if one or more given attributes
     * are different from the sc's.
     * If the returned scope != sc, the caller should pop
     * the scope after it used.
     */
    extern (D) static Scope* createNewScope(Scope* sc, StorageClass stc, LINK linkage,
        CPPMANGLE cppmangle, Visibility visibility, int explicitVisibility,
        AlignDeclaration aligndecl, PragmaDeclaration inlining)
    {
        Scope* sc2 = sc;
        if (stc != sc.stc ||
            linkage != sc.linkage ||
            cppmangle != sc.cppmangle ||
            explicitVisibility != sc.explicitVisibility ||
            visibility != sc.visibility ||
            aligndecl !is sc.aligndecl ||
            inlining != sc.inlining)
        {
            // create new one for changes
            sc2 = sc.copy();
            sc2.stc = stc;
            sc2.linkage = linkage;
            sc2.cppmangle = cppmangle;
            sc2.visibility = visibility;
            sc2.explicitVisibility = explicitVisibility;
            sc2.aligndecl = aligndecl;
            sc2.inlining = inlining;
        }
        return sc2;
    }

    override const(char)* kind() const
    {
        return "attribute";
    }

    override bool oneMember(out Dsymbol ps, Identifier ident)
    {
        Dsymbols* d = this.include(null);
        return Dsymbol.oneMembers(d, ps, ident);
    }

    override final bool hasPointers()
    {
        return this.include(null).foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
    }

    override final bool hasStaticCtorOrDtor()
    {
        return this.include(null).foreachDsymbol( (s) { return s.hasStaticCtorOrDtor(); } ) != 0;
    }

    override final void checkCtorConstInit()
    {
        this.include(null).foreachDsymbol( s => s.checkCtorConstInit() );
    }

    /****************************************
     */
    override final void addObjcSymbols(ClassDeclarations* classes, ClassDeclarations* categories)
    {
        objc.addSymbols(this, classes, categories);
    }

    override inout(AttribDeclaration) isAttribDeclaration() inout pure @safe
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Storage classes applied to Dsymbols, e.g. `const int i;`
 *
 * <stc> <decl...>
 */
extern (C++) class StorageClassDeclaration : AttribDeclaration
{
    StorageClass stc;

    extern (D) this(StorageClass stc, Dsymbols* decl) @safe
    {
        super(decl);
        this.stc = stc;
    }

    extern (D) this(const ref Loc loc, StorageClass stc, Dsymbols* decl) @safe
    {
        super(loc, decl);
        this.stc = stc;
    }

    override StorageClassDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new StorageClassDeclaration(stc, Dsymbol.arraySyntaxCopy(decl));
    }

    override final bool oneMember(out Dsymbol ps, Identifier ident)
    {
        bool t = Dsymbol.oneMembers(decl, ps, ident);
        if (t && ps)
        {
            /* This is to deal with the following case:
             * struct Tick {
             *   template to(T) { const T to() { ... } }
             * }
             * For eponymous function templates, the 'const' needs to get attached to 'to'
             * before the semantic analysis of 'to', so that template overloading based on the
             * 'this' pointer can be successful.
             */
            if (FuncDeclaration fd = ps.isFuncDeclaration())
            {
                /* Use storage_class2 instead of storage_class otherwise when we do .di generation
                 * we'll wind up with 'const const' rather than 'const'.
                 */
                /* Don't think we need to worry about mutually exclusive storage classes here
                 */
                fd.storage_class2 |= stc;
            }
        }
        return t;
    }

    override inout(StorageClassDeclaration) isStorageClassDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Deprecation with an additional message applied to Dsymbols,
 * e.g. `deprecated("Superseeded by foo") int bar;`.
 * (Note that `deprecated int bar;` is currently represented as a
 * StorageClassDeclaration with STC.deprecated_)
 *
 * `deprecated(<msg>) <decl...>`
 */
extern (C++) final class DeprecatedDeclaration : StorageClassDeclaration
{
    Expression msg;         /// deprecation message
    const(char)* msgstr;    /// cached string representation of msg

    extern (D) this(Expression msg, Dsymbols* decl) @safe
    {
        super(STC.deprecated_, decl);
        this.msg = msg;
    }

    override DeprecatedDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new DeprecatedDeclaration(msg.syntaxCopy(), Dsymbol.arraySyntaxCopy(decl));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Linkage attribute applied to Dsymbols, e.g.
 * `extern(C) void foo()`.
 *
 * `extern(<linkage>) <decl...>`
 */
extern (C++) final class LinkDeclaration : AttribDeclaration
{
    LINK linkage; /// either explicitly set or `default_`

    extern (D) this(const ref Loc loc, LINK linkage, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        //printf("LinkDeclaration(linkage = %d, decl = %p)\n", linkage, decl);
        this.linkage = linkage;
    }

    static LinkDeclaration create(const ref Loc loc, LINK p, Dsymbols* decl) @safe
    {
        return new LinkDeclaration(loc, p, decl);
    }

    override LinkDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new LinkDeclaration(loc, linkage, Dsymbol.arraySyntaxCopy(decl));
    }


    override const(char)* toChars() const
    {
        return toString().ptr;
    }

    extern(D) override const(char)[] toString() const
    {
        return "extern ()";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Attribute declaring whether an external aggregate should be mangled as
 * a struct or class in C++, e.g. `extern(C++, struct) class C { ... }`.
 * This is required for correct name mangling on MSVC targets,
 * see cppmanglewin.d for details.
 *
 * `extern(C++, <cppmangle>) <decl...>`
 */
extern (C++) final class CPPMangleDeclaration : AttribDeclaration
{
    CPPMANGLE cppmangle;

    extern (D) this(const ref Loc loc, CPPMANGLE cppmangle, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        //printf("CPPMangleDeclaration(cppmangle = %d, decl = %p)\n", cppmangle, decl);
        this.cppmangle = cppmangle;
    }

    override CPPMangleDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new CPPMangleDeclaration(loc, cppmangle, Dsymbol.arraySyntaxCopy(decl));
    }

    override const(char)* toChars() const
    {
        return toString().ptr;
    }

    extern(D) override const(char)[] toString() const
    {
        return "extern ()";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * A node to represent an `extern(C++)` namespace attribute
 *
 * There are two ways to declarate a symbol as member of a namespace:
 * `Nspace` and `CPPNamespaceDeclaration`.
 * The former creates a scope for the symbol, and inject them in the
 * parent scope at the same time.
 * The later, this class, has no semantic implications and is only
 * used for mangling.
 * Additionally, this class allows one to use reserved identifiers
 * (D keywords) in the namespace.
 *
 * A `CPPNamespaceDeclaration` can be created from an `Identifier`
 * (already resolved) or from an `Expression`, which is CTFE-ed
 * and can be either a `TupleExp`, in which can additional
 * `CPPNamespaceDeclaration` nodes are created, or a `StringExp`.
 *
 * Note that this class, like `Nspace`, matches only one identifier
 * part of a namespace. For the namespace `"foo::bar"`,
 * the will be a `CPPNamespaceDeclaration` with its `ident`
 * set to `"bar"`, and its `namespace` field pointing to another
 * `CPPNamespaceDeclaration` with its `ident` set to `"foo"`.
 */
extern (C++) final class CPPNamespaceDeclaration : AttribDeclaration
{
    /// CTFE-able expression, resolving to `TupleExp` or `StringExp`
    Expression exp;

    extern (D) this(const ref Loc loc, Identifier ident, Dsymbols* decl) @safe
    {
        super(loc, ident, decl);
    }

    extern (D) this(const ref Loc loc, Expression exp, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        this.exp = exp;
    }

    extern (D) this(const ref Loc loc, Identifier ident, Expression exp, Dsymbols* decl,
                    CPPNamespaceDeclaration parent) @safe
    {
        super(loc, ident, decl);
        this.exp = exp;
        this.cppnamespace = parent;
    }

    override CPPNamespaceDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new CPPNamespaceDeclaration(
            this.loc, this.ident, this.exp, Dsymbol.arraySyntaxCopy(this.decl), this.cppnamespace);
    }

    override const(char)* toChars() const
    {
        return toString().ptr;
    }

    extern(D) override const(char)[] toString() const
    {
        return "extern (C++, `namespace`)";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    override inout(CPPNamespaceDeclaration) isCPPNamespaceDeclaration() inout { return this; }
}

/***********************************************************
 * Visibility declaration for Dsymbols, e.g. `public int i;`
 *
 * `<visibility> <decl...>` or
 * `package(<pkg_identifiers>) <decl...>` if `pkg_identifiers !is null`
 */
extern (C++) final class VisibilityDeclaration : AttribDeclaration
{
    Visibility visibility;          /// the visibility
    Identifier[] pkg_identifiers;   /// identifiers for `package(foo.bar)` or null

    /**
     * Params:
     *  loc = source location of attribute token
     *  visibility = visibility attribute data
     *  decl = declarations which are affected by this visibility attribute
     */
    extern (D) this(const ref Loc loc, Visibility visibility, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        this.visibility = visibility;
        //printf("decl = %p\n", decl);
    }

    /**
     * Params:
     *  loc = source location of attribute token
     *  pkg_identifiers = list of identifiers for a qualified package name
     *  decl = declarations which are affected by this visibility attribute
     */
    extern (D) this(const ref Loc loc, Identifier[] pkg_identifiers, Dsymbols* decl)
    {
        super(loc, null, decl);
        this.visibility.kind = Visibility.Kind.package_;
        this.pkg_identifiers = pkg_identifiers;
        if (pkg_identifiers.length > 0)
        {
            Dsymbol tmp;
            Package.resolve(pkg_identifiers, &tmp, null);
            visibility.pkg = tmp ? tmp.isPackage() : null;
        }
    }

    override VisibilityDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);

        if (visibility.kind == Visibility.Kind.package_)
            return new VisibilityDeclaration(this.loc, pkg_identifiers, Dsymbol.arraySyntaxCopy(decl));
        else
            return new VisibilityDeclaration(this.loc, visibility, Dsymbol.arraySyntaxCopy(decl));
    }

    override const(char)* kind() const
    {
        return "visibility attribute";
    }

    override const(char)* toPrettyChars(bool)
    {
        assert(visibility.kind > Visibility.Kind.undefined);
        OutBuffer buf;
        visibilityToBuffer(buf, visibility);
        return buf.extractChars();
    }

    override inout(VisibilityDeclaration) isVisibilityDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Alignment attribute for aggregates, members and variables.
 *
 * `align(<ealign>) <decl...>` or
 * `align <decl...>` if `ealign` is null
 */
extern (C++) final class AlignDeclaration : AttribDeclaration
{
    Expressions* exps;                              /// Expression(s) yielding the desired alignment,
                                                    /// the largest value wins
    /// the actual alignment is Unknown until it's either set to the value of `ealign`
    /// or the default if `ealign` is null ( / an error ocurred)
    structalign_t salign;


    extern (D) this(const ref Loc loc, Expression exp, Dsymbols* decl)
    {
        super(loc, null, decl);
        if (exp)
        {
            exps = new Expressions();
            exps.push(exp);
        }
    }

    extern (D) this(const ref Loc loc, Expressions* exps, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        this.exps = exps;
    }

    extern (D) this(const ref Loc loc, structalign_t salign, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        this.salign = salign;
    }

    override AlignDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new AlignDeclaration(loc,
            Expression.arraySyntaxCopy(exps),
            Dsymbol.arraySyntaxCopy(decl));
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * An anonymous struct/union (defined by `isunion`).
 */
extern (C++) final class AnonDeclaration : AttribDeclaration
{
    bool isunion;           /// whether it's a union
    int sem;                /// 1 if successful semantic()
    uint anonoffset;        /// offset of anonymous struct
    uint anonstructsize;    /// size of anonymous struct
    uint anonalignsize;     /// size of anonymous struct for alignment purposes

    extern (D) this(const ref Loc loc, bool isunion, Dsymbols* decl) @safe
    {
        super(loc, null, decl);
        this.isunion = isunion;
    }

    override AnonDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new AnonDeclaration(loc, isunion, Dsymbol.arraySyntaxCopy(decl));
    }

    override const(char)* kind() const
    {
        return (isunion ? "anonymous union" : "anonymous struct");
    }

    override inout(AnonDeclaration) isAnonDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Pragma applied to Dsymbols, e.g. `pragma(inline, true) void foo`,
 * but not PragmaStatement's like `pragma(msg, "hello");`.
 *
 * pragma(<ident>, <args>)
 */
extern (C++) final class PragmaDeclaration : AttribDeclaration
{
    Expressions* args;      /// parameters of this pragma

    extern (D) this(const ref Loc loc, Identifier ident, Expressions* args, Dsymbols* decl) @safe
    {
        super(loc, ident, decl);
        this.args = args;
    }

    override PragmaDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("PragmaDeclaration::syntaxCopy(%s)\n", toChars());
        assert(!s);
        return new PragmaDeclaration(loc, ident, Expression.arraySyntaxCopy(args), Dsymbol.arraySyntaxCopy(decl));
    }

    override const(char)* kind() const
    {
        return "pragma";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * A conditional compilation declaration, used for `version`
 * / `debug` and specialized for `static if`.
 *
 * <condition> { <decl...> } else { <elsedecl> }
 */
extern (C++) class ConditionalDeclaration : AttribDeclaration
{
    Condition condition;    /// condition deciding whether decl or elsedecl applies
    Dsymbols* elsedecl;     /// array of Dsymbol's for else block

    extern (D) this(const ref Loc loc, Condition condition, Dsymbols* decl, Dsymbols* elsedecl) @safe
    {
        super(loc, null, decl);
        //printf("ConditionalDeclaration::ConditionalDeclaration()\n");
        this.condition = condition;
        this.elsedecl = elsedecl;
    }

    override ConditionalDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new ConditionalDeclaration(loc, condition.syntaxCopy(), Dsymbol.arraySyntaxCopy(decl), Dsymbol.arraySyntaxCopy(elsedecl));
    }

    override final bool oneMember(out Dsymbol ps, Identifier ident)
    {
        //printf("ConditionalDeclaration::oneMember(), inc = %d\n", condition.inc);
        if (condition.inc != Include.notComputed)
        {
            Dsymbols* d = condition.include(null) ? decl : elsedecl;
            return Dsymbol.oneMembers(d, ps, ident);
        }
        else
        {
            bool res = (Dsymbol.oneMembers(decl, ps, ident) && ps is null && Dsymbol.oneMembers(elsedecl, ps, ident) && ps is null);
            ps = null;
            return res;
        }
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * `<scopesym> {
 *      static if (<condition>) { <decl> } else { <elsedecl> }
 * }`
 */
extern (C++) final class StaticIfDeclaration : ConditionalDeclaration
{
    ScopeDsymbol scopesym;          /// enclosing symbol (e.g. module) where symbols will be inserted
    bool addisdone = false; /// true if members have been added to scope
    bool onStack = false;   /// true if a call to `include` is currently active

    extern (D) this(const ref Loc loc, Condition condition, Dsymbols* decl, Dsymbols* elsedecl) @safe
    {
        super(loc, condition, decl, elsedecl);
        //printf("StaticIfDeclaration::StaticIfDeclaration()\n");
    }

    override StaticIfDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new StaticIfDeclaration(loc, condition.syntaxCopy(), Dsymbol.arraySyntaxCopy(decl), Dsymbol.arraySyntaxCopy(elsedecl));
    }

    override const(char)* kind() const
    {
        return "static if";
    }

    override inout(StaticIfDeclaration) isStaticIfDeclaration() inout pure @safe
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Static foreach at declaration scope, like:
 *     static foreach (i; [0, 1, 2]){ }
 */

extern (C++) final class StaticForeachDeclaration : AttribDeclaration
{
    StaticForeach sfe; /// contains `static foreach` expansion logic

    ScopeDsymbol scopesym; /// cached enclosing scope (mimics `static if` declaration)

    /++
     `include` can be called multiple times, but a `static foreach`
     should be expanded at most once.  Achieved by caching the result
     of the first call.  We need both `cached` and `cache`, because
     `null` is a valid value for `cache`.
     +/
    bool onStack = false;
    bool cached = false;
    Dsymbols* cache = null;

    extern (D) this(StaticForeach sfe, Dsymbols* decl) @safe
    {
        super(sfe.loc, null, decl);
        this.sfe = sfe;
    }

    override StaticForeachDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new StaticForeachDeclaration(
            sfe.syntaxCopy(),
            Dsymbol.arraySyntaxCopy(decl));
    }

    override bool oneMember(out Dsymbol ps, Identifier ident)
    {
        // Required to support IFTI on a template that contains a
        // `static foreach` declaration.  `super.oneMember` calls
        // include with a `null` scope.  As `static foreach` requires
        // the scope for expansion, `oneMember` can only return a
        // precise result once `static foreach` has been expanded.
        if (cached)
        {
            return super.oneMember(ps, ident);
        }
        ps = null; // a `static foreach` declaration may in general expand to multiple symbols
        return false;
    }

    override const(char)* kind() const
    {
        return "static foreach";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Collection of declarations that stores foreach index variables in a
 * local symbol table.  Other symbols declared within are forwarded to
 * another scope, like:
 *
 *      static foreach (i; 0 .. 10) // loop variables for different indices do not conflict.
 *      { // this body is expanded into 10 ForwardingAttribDeclarations, where `i` has storage class STC.local
 *          mixin("enum x" ~ to!string(i) ~ " = i"); // ok, can access current loop variable
 *      }
 *
 *      static foreach (i; 0.. 10)
 *      {
 *          pragma(msg, mixin("x" ~ to!string(i))); // ok, all 10 symbols are visible as they were forwarded to the global scope
 *      }
 *
 *      static assert(!is(typeof(i))); // loop index variable is not visible outside of the static foreach loop
 *
 * A StaticForeachDeclaration generates one
 * ForwardingAttribDeclaration for each expansion of its body.  The
 * AST of the ForwardingAttribDeclaration contains both the `static
 * foreach` variables and the respective copy of the `static foreach`
 * body.  The functionality is achieved by using a
 * ForwardingScopeDsymbol as the parent symbol for the generated
 * declarations.
 */

extern(C++) final class ForwardingAttribDeclaration : AttribDeclaration
{
    ForwardingScopeDsymbol sym = null;

    this(Dsymbols* decl) @safe
    {
        super(decl);
        sym = new ForwardingScopeDsymbol();
        sym.symtab = new DsymbolTable();
    }


    override inout(ForwardingAttribDeclaration) isForwardingAttribDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Mixin declarations, like:
 *      mixin("int x");
 * https://dlang.org/spec/module.html#mixin-declaration
 */
// Note: was CompileDeclaration
extern (C++) final class MixinDeclaration : AttribDeclaration
{
    Expressions* exps;
    ScopeDsymbol scopesym;
    bool compiled;

    extern (D) this(const ref Loc loc, Expressions* exps) @safe
    {
        super(loc, null, null);
        //printf("MixinDeclaration(loc = %d)\n", loc.linnum);
        this.exps = exps;
    }

    override MixinDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("MixinDeclaration::syntaxCopy('%s')\n", toChars());
        return new MixinDeclaration(loc, Expression.arraySyntaxCopy(exps));
    }

    override const(char)* kind() const
    {
        return "mixin";
    }

    override inout(MixinDeclaration) isMixinDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * User defined attributes look like:
 *      @foo(args, ...)
 *      @(args, ...)
 */
extern (C++) final class UserAttributeDeclaration : AttribDeclaration
{
    Expressions* atts;

    extern (D) this(Expressions* atts, Dsymbols* decl) @safe
    {
        super(decl);
        this.atts = atts;
    }

    override UserAttributeDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("UserAttributeDeclaration::syntaxCopy('%s')\n", toChars());
        assert(!s);
        return new UserAttributeDeclaration(Expression.arraySyntaxCopy(this.atts), Dsymbol.arraySyntaxCopy(decl));
    }

    extern (D) static Expressions* concat(Expressions* udas1, Expressions* udas2)
    {
        Expressions* udas;
        if (!udas1 || udas1.length == 0)
            udas = udas2;
        else if (!udas2 || udas2.length == 0)
            udas = udas1;
        else
        {
            /* Create a new tuple that combines them
             * (do not append to left operand, as this is a copy-on-write operation)
             */
            udas = new Expressions(2);
            (*udas)[0] = new TupleExp(Loc.initial, udas1);
            (*udas)[1] = new TupleExp(Loc.initial, udas2);
        }
        return udas;
    }

    override const(char)* kind() const
    {
        return "UserAttribute";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * Returns `true` if the given symbol is a symbol declared in
 * `core.attribute` and has the given identifier.
 *
 * This is used to determine if a symbol is a UDA declared in
 * `core.attribute`.
 *
 * Params:
 *  sym = the symbol to check
 *  ident = the name of the expected UDA
 */
bool isCoreUda(Dsymbol sym, Identifier ident)
{
    if (sym.ident != ident || !sym.parent)
        return false;

    auto _module = sym.parent.isModule();
    return _module && _module.isCoreModule(Id.attribute);
}

/**
 * Iterates the UDAs attached to the given symbol, without performing semantic
 * analysis.
 *
 * Use this function instead of `foreachUda` if semantic analysis of `sym` is
 * still in progress.
 *
 * Params:
 *  sym = the symbol to get the UDAs from
 *  dg = called once for each UDA
 *
 * Returns:
 *  If `dg` returns `!= 0`, stops the iteration and returns that value.
 *  Otherwise, returns 0.
 */
int foreachUdaNoSemantic(Dsymbol sym, int delegate(Expression) dg)
{
    if (sym.userAttribDecl is null || sym.userAttribDecl.atts is null)
        return 0;

    foreach (exp; *sym.userAttribDecl.atts)
    {
        if (auto result = dg(exp))
            return result;
    }

    return 0;
}

/**
 * Returns: true if the given expression is an enum from `core.attribute` named `id`
 */
bool isEnumAttribute(Expression e, Identifier id)
{
    import dmd.attrib : isCoreUda;
    import dmd.id : Id;

    // Logic based on dmd.objc.Supported.declaredAsOptionalCount
    auto typeExp = e.isTypeExp;
    if (!typeExp)
        return false;

    auto typeEnum = typeExp.type.isTypeEnum();
    if (!typeEnum)
        return false;

    if (isCoreUda(typeEnum.sym, id))
        return true;

    return false;
}
