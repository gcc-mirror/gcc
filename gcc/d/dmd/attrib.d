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
 * Copyright:   Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
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
import dmd.dsymbolsem : dsymbolSemantic;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.hdrgen : visibilityToBuffer;
import dmd.id;
import dmd.identifier;
import dmd.mtype;
import dmd.objc; // for objc.addSymbols
import dmd.common.outbuffer;
import dmd.root.array; // for each
import dmd.tokens;
import dmd.visitor;

/***********************************************************
 * Abstract attribute applied to Dsymbol's used as a common
 * ancestor for storage classes (StorageClassDeclaration),
 * linkage (LinkageDeclaration) and others.
 */
extern (C++) abstract class AttribDeclaration : Dsymbol
{
    Dsymbols* decl;     /// Dsymbol's affected by this AttribDeclaration

    extern (D) this(Dsymbols* decl)
    {
        this.decl = decl;
    }

    extern (D) this(const ref Loc loc, Identifier ident, Dsymbols* decl)
    {
        super(loc, ident);
        this.decl = decl;
    }

    Dsymbols* include(Scope* sc)
    {
        if (errors)
            return null;

        return decl;
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

    /****************************************
     * A hook point to supply scope for members.
     * addMember, setScope, importAll, semantic, semantic2 and semantic3 will use this.
     */
    Scope* newScope(Scope* sc)
    {
        return sc;
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        Dsymbols* d = include(sc);
        if (d)
        {
            Scope* sc2 = newScope(sc);
            d.foreachDsymbol( s => s.addMember(sc2, sds) );
            if (sc2 != sc)
                sc2.pop();
        }
    }

    override void setScope(Scope* sc)
    {
        Dsymbols* d = include(sc);
        //printf("\tAttribDeclaration::setScope '%s', d = %p\n",toChars(), d);
        if (d)
        {
            Scope* sc2 = newScope(sc);
            d.foreachDsymbol( s => s.setScope(sc2) );
            if (sc2 != sc)
                sc2.pop();
        }
    }

    override void importAll(Scope* sc)
    {
        Dsymbols* d = include(sc);
        //printf("\tAttribDeclaration::importAll '%s', d = %p\n", toChars(), d);
        if (d)
        {
            Scope* sc2 = newScope(sc);
            d.foreachDsymbol( s => s.importAll(sc2) );
            if (sc2 != sc)
                sc2.pop();
        }
    }

    override void addComment(const(char)* comment)
    {
        //printf("AttribDeclaration::addComment %s\n", comment);
        if (comment)
        {
            include(null).foreachDsymbol( s => s.addComment(comment) );
        }
    }

    override const(char)* kind() const
    {
        return "attribute";
    }

    override bool oneMember(Dsymbol* ps, Identifier ident)
    {
        Dsymbols* d = include(null);
        return Dsymbol.oneMembers(d, ps, ident);
    }

    override void setFieldOffset(AggregateDeclaration ad, ref FieldState fieldState, bool isunion)
    {
        include(null).foreachDsymbol( s => s.setFieldOffset(ad, fieldState, isunion) );
    }

    override final bool hasPointers()
    {
        return include(null).foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
    }

    override final bool hasStaticCtorOrDtor()
    {
        return include(null).foreachDsymbol( (s) { return s.hasStaticCtorOrDtor(); } ) != 0;
    }

    override final void checkCtorConstInit()
    {
        include(null).foreachDsymbol( s => s.checkCtorConstInit() );
    }

    /****************************************
     */
    override final void addLocalClass(ClassDeclarations* aclasses)
    {
        include(null).foreachDsymbol( s => s.addLocalClass(aclasses) );
    }

    override final void addObjcSymbols(ClassDeclarations* classes, ClassDeclarations* categories)
    {
        objc.addSymbols(this, classes, categories);
    }

    override final inout(AttribDeclaration) isAttribDeclaration() inout pure @safe
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

    extern (D) this(StorageClass stc, Dsymbols* decl)
    {
        super(decl);
        this.stc = stc;
    }

    override StorageClassDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new StorageClassDeclaration(stc, Dsymbol.arraySyntaxCopy(decl));
    }

    override Scope* newScope(Scope* sc)
    {
        StorageClass scstc = sc.stc;
        /* These sets of storage classes are mutually exclusive,
         * so choose the innermost or most recent one.
         */
        if (stc & (STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.manifest))
            scstc &= ~(STC.auto_ | STC.scope_ | STC.static_ | STC.extern_ | STC.manifest);
        if (stc & (STC.auto_ | STC.scope_ | STC.static_ | STC.manifest | STC.gshared))
            scstc &= ~(STC.auto_ | STC.scope_ | STC.static_ | STC.manifest | STC.gshared);
        if (stc & (STC.const_ | STC.immutable_ | STC.manifest))
            scstc &= ~(STC.const_ | STC.immutable_ | STC.manifest);
        if (stc & (STC.gshared | STC.shared_))
            scstc &= ~(STC.gshared | STC.shared_);
        if (stc & (STC.safe | STC.trusted | STC.system))
            scstc &= ~(STC.safe | STC.trusted | STC.system);
        scstc |= stc;
        //printf("scstc = x%llx\n", scstc);
        return createNewScope(sc, scstc, sc.linkage, sc.cppmangle,
            sc.visibility, sc.explicitVisibility, sc.aligndecl, sc.inlining);
    }

    override final bool oneMember(Dsymbol* ps, Identifier ident)
    {
        bool t = Dsymbol.oneMembers(decl, ps, ident);
        if (t && *ps)
        {
            /* This is to deal with the following case:
             * struct Tick {
             *   template to(T) { const T to() { ... } }
             * }
             * For eponymous function templates, the 'const' needs to get attached to 'to'
             * before the semantic analysis of 'to', so that template overloading based on the
             * 'this' pointer can be successful.
             */
            FuncDeclaration fd = (*ps).isFuncDeclaration();
            if (fd)
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

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        Dsymbols* d = include(sc);
        if (d)
        {
            Scope* sc2 = newScope(sc);

            d.foreachDsymbol( (s)
            {
                //printf("\taddMember %s to %s\n", s.toChars(), sds.toChars());
                // STC.local needs to be attached before the member is added to the scope (because it influences the parent symbol)
                if (auto decl = s.isDeclaration())
                {
                    decl.storage_class |= stc & STC.local;
                    if (auto sdecl = s.isStorageClassDeclaration()) // TODO: why is this not enough to deal with the nested case?
                    {
                        sdecl.stc |= stc & STC.local;
                    }
                }
                s.addMember(sc2, sds);
            });

            if (sc2 != sc)
                sc2.pop();
        }

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

    extern (D) this(Expression msg, Dsymbols* decl)
    {
        super(STC.deprecated_, decl);
        this.msg = msg;
    }

    override DeprecatedDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new DeprecatedDeclaration(msg.syntaxCopy(), Dsymbol.arraySyntaxCopy(decl));
    }

    /**
     * Provides a new scope with `STC.deprecated_` and `Scope.depdecl` set
     *
     * Calls `StorageClassDeclaration.newScope` (as it must be called or copied
     * in any function overriding `newScope`), then set the `Scope`'s depdecl.
     *
     * Returns:
     *   Always a new scope, to use for this `DeprecatedDeclaration`'s members.
     */
    override Scope* newScope(Scope* sc)
    {
        auto scx = super.newScope(sc);
        // The enclosing scope is deprecated as well
        if (scx == sc)
            scx = sc.push();
        scx.depdecl = this;
        return scx;
    }

    override void setScope(Scope* sc)
    {
        //printf("DeprecatedDeclaration::setScope() %p\n", this);
        if (decl)
            Dsymbol.setScope(sc); // for forward reference
        return AttribDeclaration.setScope(sc);
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

    extern (D) this(const ref Loc loc, LINK linkage, Dsymbols* decl)
    {
        super(loc, null, decl);
        //printf("LinkDeclaration(linkage = %d, decl = %p)\n", linkage, decl);
        this.linkage = linkage;
    }

    static LinkDeclaration create(const ref Loc loc, LINK p, Dsymbols* decl)
    {
        return new LinkDeclaration(loc, p, decl);
    }

    override LinkDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new LinkDeclaration(loc, linkage, Dsymbol.arraySyntaxCopy(decl));
    }

    override Scope* newScope(Scope* sc)
    {
        return createNewScope(sc, sc.stc, this.linkage, sc.cppmangle, sc.visibility, sc.explicitVisibility,
            sc.aligndecl, sc.inlining);
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

    extern (D) this(const ref Loc loc, CPPMANGLE cppmangle, Dsymbols* decl)
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

    override Scope* newScope(Scope* sc)
    {
        return createNewScope(sc, sc.stc, LINK.cpp, cppmangle, sc.visibility, sc.explicitVisibility,
            sc.aligndecl, sc.inlining);
    }

    override void setScope(Scope* sc)
    {
        if (decl)
            Dsymbol.setScope(sc); // for forward reference
        return AttribDeclaration.setScope(sc);
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

    extern (D) this(const ref Loc loc, Identifier ident, Dsymbols* decl)
    {
        super(loc, ident, decl);
    }

    extern (D) this(const ref Loc loc, Expression exp, Dsymbols* decl)
    {
        super(loc, null, decl);
        this.exp = exp;
    }

    extern (D) this(const ref Loc loc, Identifier ident, Expression exp, Dsymbols* decl,
                    CPPNamespaceDeclaration parent)
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

    /**
     * Returns:
     *   A copy of the parent scope, with `this` as `namespace` and C++ linkage
     */
    override Scope* newScope(Scope* sc)
    {
        auto scx = sc.copy();
        scx.linkage = LINK.cpp;
        scx.namespace = this;
        return scx;
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
    extern (D) this(const ref Loc loc, Visibility visibility, Dsymbols* decl)
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

    override Scope* newScope(Scope* sc)
    {
        if (pkg_identifiers)
            dsymbolSemantic(this, sc);
        return createNewScope(sc, sc.stc, sc.linkage, sc.cppmangle, this.visibility, 1, sc.aligndecl, sc.inlining);
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        if (pkg_identifiers)
        {
            Dsymbol tmp;
            Package.resolve(pkg_identifiers, &tmp, null);
            visibility.pkg = tmp ? tmp.isPackage() : null;
            pkg_identifiers = null;
        }
        if (visibility.kind == Visibility.Kind.package_ && visibility.pkg && sc._module)
        {
            Module m = sc._module;

            // While isAncestorPackageOf does an equality check, the fix for issue 17441 adds a check to see if
            // each package's .isModule() properites are equal.
            //
            // Properties generated from `package(foo)` i.e. visibility.pkg have .isModule() == null.
            // This breaks package declarations of the package in question if they are declared in
            // the same package.d file, which _do_ have a module associated with them, and hence a non-null
            // isModule()
            if (!m.isPackage() || !visibility.pkg.ident.equals(m.isPackage().ident))
            {
                Package pkg = m.parent ? m.parent.isPackage() : null;
                if (!pkg || !visibility.pkg.isAncestorPackageOf(pkg))
                    error("does not bind to one of ancestor packages of module `%s`", m.toPrettyChars(true));
            }
        }
        return AttribDeclaration.addMember(sc, sds);
    }

    override const(char)* kind() const
    {
        return "visibility attribute";
    }

    override const(char)* toPrettyChars(bool)
    {
        assert(visibility.kind > Visibility.Kind.undefined);
        OutBuffer buf;
        visibilityToBuffer(&buf, visibility);
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

    extern (D) this(const ref Loc loc, Expressions* exps, Dsymbols* decl)
    {
        super(loc, null, decl);
        this.exps = exps;
    }

    extern (D) this(const ref Loc loc, structalign_t salign, Dsymbols* decl)
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

    override Scope* newScope(Scope* sc)
    {
        return createNewScope(sc, sc.stc, sc.linkage, sc.cppmangle, sc.visibility, sc.explicitVisibility, this, sc.inlining);
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

    extern (D) this(const ref Loc loc, bool isunion, Dsymbols* decl)
    {
        super(loc, null, decl);
        this.isunion = isunion;
    }

    override AnonDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new AnonDeclaration(loc, isunion, Dsymbol.arraySyntaxCopy(decl));
    }

    override void setScope(Scope* sc)
    {
        if (decl)
            Dsymbol.setScope(sc);
        return AttribDeclaration.setScope(sc);
    }

    override void setFieldOffset(AggregateDeclaration ad, ref FieldState fieldState, bool isunion)
    {
        //printf("\tAnonDeclaration::setFieldOffset %s %p\n", isunion ? "union" : "struct", this);
        if (decl)
        {
            /* This works by treating an AnonDeclaration as an aggregate 'member',
             * so in order to place that member we need to compute the member's
             * size and alignment.
             */
            size_t fieldstart = ad.fields.dim;

            /* Hackishly hijack ad's structsize and alignsize fields
             * for use in our fake anon aggregate member.
             */
            uint savestructsize = ad.structsize;
            uint savealignsize = ad.alignsize;
            ad.structsize = 0;
            ad.alignsize = 0;

            FieldState fs;
            decl.foreachDsymbol( (s)
            {
                s.setFieldOffset(ad, fs, this.isunion);
                if (this.isunion)
                    fs.offset = 0;
            });

            /* https://issues.dlang.org/show_bug.cgi?id=13613
             * If the fields in this.members had been already
             * added in ad.fields, just update *poffset for the subsequent
             * field offset calculation.
             */
            if (fieldstart == ad.fields.dim)
            {
                ad.structsize = savestructsize;
                ad.alignsize = savealignsize;
                fieldState.offset = ad.structsize;
                return;
            }

            anonstructsize = ad.structsize;
            anonalignsize = ad.alignsize;
            ad.structsize = savestructsize;
            ad.alignsize = savealignsize;

            // 0 sized structs are set to 1 byte
            if (anonstructsize == 0)
            {
                anonstructsize = 1;
                anonalignsize = 1;
            }

            assert(_scope);
            auto alignment = _scope.alignment();

            /* Given the anon 'member's size and alignment,
             * go ahead and place it.
             */
            anonoffset = AggregateDeclaration.placeField(
                &fieldState.offset,
                anonstructsize, anonalignsize, alignment,
                &ad.structsize, &ad.alignsize,
                isunion);

            // Add to the anon fields the base offset of this anonymous aggregate
            //printf("anon fields, anonoffset = %d\n", anonoffset);
            foreach (const i; fieldstart .. ad.fields.dim)
            {
                VarDeclaration v = ad.fields[i];
                //printf("\t[%d] %s %d\n", i, v.toChars(), v.offset);
                v.offset += anonoffset;
            }
        }
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

    extern (D) this(const ref Loc loc, Identifier ident, Expressions* args, Dsymbols* decl)
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

    override Scope* newScope(Scope* sc)
    {
        if (ident == Id.Pinline)
        {
            // We keep track of this pragma inside scopes,
            // then it's evaluated on demand in function semantic
            return createNewScope(sc, sc.stc, sc.linkage, sc.cppmangle, sc.visibility, sc.explicitVisibility, sc.aligndecl, this);
        }
        if (ident == Id.printf || ident == Id.scanf)
        {
            auto sc2 = sc.push();

            if (ident == Id.printf)
                // Override previous setting, never let both be set
                sc2.flags = (sc2.flags & ~SCOPE.scanf) | SCOPE.printf;
            else
                sc2.flags = (sc2.flags & ~SCOPE.printf) | SCOPE.scanf;

            return sc2;
        }
        return sc;
    }

    PINLINE evalPragmaInline(Scope* sc)
    {
        if (!args || args.dim == 0)
            return PINLINE.default_;

        Expression e = (*args)[0];
        if (!e.type)
        {

            sc = sc.startCTFE();
            e = e.expressionSemantic(sc);
            e = resolveProperties(sc, e);
            sc = sc.endCTFE();
            e = e.ctfeInterpret();
            e = e.toBoolean(sc);
            if (e.isErrorExp())
                error("pragma(`inline`, `true` or `false`) expected, not `%s`", (*args)[0].toChars());
            (*args)[0] = e;
        }

        const opt = e.toBool();
        if (opt.isEmpty())
            return PINLINE.default_;
        else if (opt.get())
            return PINLINE.always;
        else
            return PINLINE.never;
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

    extern (D) this(const ref Loc loc, Condition condition, Dsymbols* decl, Dsymbols* elsedecl)
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

    override final bool oneMember(Dsymbol* ps, Identifier ident)
    {
        //printf("ConditionalDeclaration::oneMember(), inc = %d\n", condition.inc);
        if (condition.inc != Include.notComputed)
        {
            Dsymbols* d = condition.include(null) ? decl : elsedecl;
            return Dsymbol.oneMembers(d, ps, ident);
        }
        else
        {
            bool res = (Dsymbol.oneMembers(decl, ps, ident) && *ps is null && Dsymbol.oneMembers(elsedecl, ps, ident) && *ps is null);
            *ps = null;
            return res;
        }
    }

    // Decide if 'then' or 'else' code should be included
    override Dsymbols* include(Scope* sc)
    {
        //printf("ConditionalDeclaration::include(sc = %p) scope = %p\n", sc, _scope);

        if (errors)
            return null;

        assert(condition);
        return condition.include(_scope ? _scope : sc) ? decl : elsedecl;
    }

    override final void addComment(const(char)* comment)
    {
        /* Because addComment is called by the parser, if we called
         * include() it would define a version before it was used.
         * But it's no problem to drill down to both decl and elsedecl,
         * so that's the workaround.
         */
        if (comment)
        {
            decl    .foreachDsymbol( s => s.addComment(comment) );
            elsedecl.foreachDsymbol( s => s.addComment(comment) );
        }
    }

    override void setScope(Scope* sc)
    {
        include(sc).foreachDsymbol( s => s.setScope(sc) );
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
    private bool addisdone = false; /// true if members have been added to scope
    private bool onStack = false;   /// true if a call to `include` is currently active

    extern (D) this(const ref Loc loc, Condition condition, Dsymbols* decl, Dsymbols* elsedecl)
    {
        super(loc, condition, decl, elsedecl);
        //printf("StaticIfDeclaration::StaticIfDeclaration()\n");
    }

    override StaticIfDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new StaticIfDeclaration(loc, condition.syntaxCopy(), Dsymbol.arraySyntaxCopy(decl), Dsymbol.arraySyntaxCopy(elsedecl));
    }

    /****************************************
     * Different from other AttribDeclaration subclasses, include() call requires
     * the completion of addMember and setScope phases.
     */
    override Dsymbols* include(Scope* sc)
    {
        //printf("StaticIfDeclaration::include(sc = %p) scope = %p\n", sc, _scope);

        if (errors || onStack)
            return null;
        onStack = true;
        scope(exit) onStack = false;

        if (sc && condition.inc == Include.notComputed)
        {
            assert(scopesym); // addMember is already done
            assert(_scope); // setScope is already done
            Dsymbols* d = ConditionalDeclaration.include(_scope);
            if (d && !addisdone)
            {
                // Add members lazily.
                d.foreachDsymbol( s => s.addMember(_scope, scopesym) );

                // Set the member scopes lazily.
                d.foreachDsymbol( s => s.setScope(_scope) );

                addisdone = true;
            }
            return d;
        }
        else
        {
            return ConditionalDeclaration.include(sc);
        }
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        //printf("StaticIfDeclaration::addMember() '%s'\n", toChars());
        /* This is deferred until the condition evaluated later (by the include() call),
         * so that expressions in the condition can refer to declarations
         * in the same scope, such as:
         *
         * template Foo(int i)
         * {
         *     const int j = i + 1;
         *     static if (j == 3)
         *         const int k;
         * }
         */
        this.scopesym = sds;
    }

    override void setScope(Scope* sc)
    {
        // do not evaluate condition before semantic pass
        // But do set the scope, in case we need it for forward referencing
        Dsymbol.setScope(sc);
    }

    override void importAll(Scope* sc)
    {
        // do not evaluate condition before semantic pass
    }

    override const(char)* kind() const
    {
        return "static if";
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

    extern (D) this(StaticForeach sfe, Dsymbols* decl)
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

    override bool oneMember(Dsymbol* ps, Identifier ident)
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
        *ps = null; // a `static foreach` declaration may in general expand to multiple symbols
        return false;
    }

    override Dsymbols* include(Scope* sc)
    {
        if (errors || onStack)
            return null;
        if (cached)
        {
            assert(!onStack);
            return cache;
        }
        onStack = true;
        scope(exit) onStack = false;

        if (_scope)
        {
            sfe.prepare(_scope); // lower static foreach aggregate
        }
        if (!sfe.ready())
        {
            return null; // TODO: ok?
        }

        // expand static foreach
        import dmd.statementsem: makeTupleForeach;
        Dsymbols* d = makeTupleForeach(_scope, true, true, sfe.aggrfe, decl, sfe.needExpansion).decl;
        if (d) // process generated declarations
        {
            // Add members lazily.
            d.foreachDsymbol( s => s.addMember(_scope, scopesym) );

            // Set the member scopes lazily.
            d.foreachDsymbol( s => s.setScope(_scope) );
        }
        cached = true;
        cache = d;
        return d;
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        // used only for caching the enclosing symbol
        this.scopesym = sds;
    }

    override void addComment(const(char)* comment)
    {
        // do nothing
        // change this to give semantics to documentation comments on static foreach declarations
    }

    override void setScope(Scope* sc)
    {
        // do not evaluate condition before semantic pass
        // But do set the scope, in case we need it for forward referencing
        Dsymbol.setScope(sc);
    }

    override void importAll(Scope* sc)
    {
        // do not evaluate aggregate before semantic pass
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
 *      static assert (!is(typeof(i))); // loop index variable is not visible outside of the static foreach loop
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

    this(Dsymbols* decl)
    {
        super(decl);
        sym = new ForwardingScopeDsymbol();
        sym.symtab = new DsymbolTable();
    }

    /**************************************
     * Use the ForwardingScopeDsymbol as the parent symbol for members.
     */
    override Scope* newScope(Scope* sc)
    {
        return sc.push(sym);
    }

    /***************************************
     * Lazily initializes the scope to forward to.
     */
    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        sym.parent = sds;
        return super.addMember(sc, sym);
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
extern (C++) final class CompileDeclaration : AttribDeclaration
{
    Expressions* exps;
    ScopeDsymbol scopesym;
    bool compiled;

    extern (D) this(const ref Loc loc, Expressions* exps)
    {
        super(loc, null, null);
        //printf("CompileDeclaration(loc = %d)\n", loc.linnum);
        this.exps = exps;
    }

    override CompileDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("CompileDeclaration::syntaxCopy('%s')\n", toChars());
        return new CompileDeclaration(loc, Expression.arraySyntaxCopy(exps));
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        //printf("CompileDeclaration::addMember(sc = %p, sds = %p, memnum = %d)\n", sc, sds, memnum);
        this.scopesym = sds;
    }

    override void setScope(Scope* sc)
    {
        Dsymbol.setScope(sc);
    }

    override const(char)* kind() const
    {
        return "mixin";
    }

    override inout(CompileDeclaration) isCompileDeclaration() inout
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

    extern (D) this(Expressions* atts, Dsymbols* decl)
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

    override Scope* newScope(Scope* sc)
    {
        Scope* sc2 = sc;
        if (atts && atts.dim)
        {
            // create new one for changes
            sc2 = sc.copy();
            sc2.userAttribDecl = this;
        }
        return sc2;
    }

    override void setScope(Scope* sc)
    {
        //printf("UserAttributeDeclaration::setScope() %p\n", this);
        if (decl)
            Dsymbol.setScope(sc); // for forward reference of UDAs
        return AttribDeclaration.setScope(sc);
    }

    extern (D) static Expressions* concat(Expressions* udas1, Expressions* udas2)
    {
        Expressions* udas;
        if (!udas1 || udas1.dim == 0)
            udas = udas2;
        else if (!udas2 || udas2.dim == 0)
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

    Expressions* getAttributes()
    {
        if (auto sc = _scope)
        {
            _scope = null;
            arrayExpressionSemantic(atts, sc);
        }
        auto exps = new Expressions();
        if (userAttribDecl && userAttribDecl !is this)
            exps.push(new TupleExp(Loc.initial, userAttribDecl.getAttributes()));
        if (atts && atts.dim)
            exps.push(new TupleExp(Loc.initial, atts));
        return exps;
    }

    override const(char)* kind() const
    {
        return "UserAttribute";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    /**
     * Check if the provided expression references `core.attribute.gnuAbiTag`
     *
     * This should be called after semantic has been run on the expression.
     * Semantic on UDA happens in semantic2 (see `dmd.semantic2`).
     *
     * Params:
     *   e = Expression to check (usually from `UserAttributeDeclaration.atts`)
     *
     * Returns:
     *   `true` if the expression references the compiler-recognized `gnuAbiTag`
     */
    static bool isGNUABITag(Expression e)
    {
        if (global.params.cplusplus < CppStdRevision.cpp11)
            return false;

        auto ts = e.type ? e.type.isTypeStruct() : null;
        if (!ts)
            return false;
        if (ts.sym.ident != Id.udaGNUAbiTag || !ts.sym.parent)
            return false;
        // Can only be defined in druntime
        Module m = ts.sym.parent.isModule();
        if (!m || !m.isCoreModule(Id.attribute))
            return false;
        return true;
    }

    /**
     * Called from a symbol's semantic to check if `gnuAbiTag` UDA
     * can be applied to them
     *
     * Directly emits an error if the UDA doesn't work with this symbol
     *
     * Params:
     *   sym = symbol to check for `gnuAbiTag`
     *   linkage = Linkage of the symbol (Declaration.link or sc.link)
     */
    static void checkGNUABITag(Dsymbol sym, LINK linkage)
    {
        if (global.params.cplusplus < CppStdRevision.cpp11)
            return;

        foreachUdaNoSemantic(sym, (exp) {
            if (isGNUABITag(exp))
            {
                if (sym.isCPPNamespaceDeclaration() || sym.isNspace())
                {
                    exp.error("`@%s` cannot be applied to namespaces", Id.udaGNUAbiTag.toChars());
                    sym.errors = true;
                }
                else if (linkage != LINK.cpp)
                {
                    exp.error("`@%s` can only apply to C++ symbols", Id.udaGNUAbiTag.toChars());
                    sym.errors = true;
                }
                // Only one `@gnuAbiTag` is allowed by semantic2
                return 1; // break
            }
            return 0; // continue
        });
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
 * Iterates the UDAs attached to the given symbol.
 *
 * Params:
 *  sym = the symbol to get the UDAs from
 *  sc = scope to use for semantic analysis of UDAs
 *  dg = called once for each UDA
 *
 * Returns:
 *  If `dg` returns `!= 0`, stops the iteration and returns that value.
 *  Otherwise, returns 0.
 */
int foreachUda(Dsymbol sym, Scope* sc, int delegate(Expression) dg)
{
    if (!sym.userAttribDecl)
        return 0;

    auto udas = sym.userAttribDecl.getAttributes();
    arrayExpressionSemantic(udas, sc, true);

    return udas.each!((uda) {
        if (!uda.isTupleExp())
            return 0;

        auto exps = uda.isTupleExp().exps;

        return exps.each!((e) {
            assert(e);

            if (auto result = dg(e))
                return result;

            return 0;
        });
    });
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
