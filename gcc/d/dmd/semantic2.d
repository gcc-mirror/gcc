/**
 * Performs the semantic2 stage, which deals with initializer expressions.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/semantic2.d, _semantic2.d)
 * Documentation:  https://dlang.org/phobos/dmd_semantic2.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/semantic2.d
 */

module dmd.semantic2;

import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.blockexit;
import dmd.timetrace;
import dmd.clone;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.denum;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.dversion;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.initsem;
import dmd.hdrgen;
import dmd.mtype;
import dmd.nogc;
import dmd.nspace;
import dmd.objc;
import dmd.opover;
import dmd.parse;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.rmem;
import dmd.root.string : toDString;
import dmd.rootobject;
import dmd.root.utf;
import dmd.sideeffect;
import dmd.statementsem;
import dmd.staticassert;
import dmd.tokens;
import dmd.statement;
import dmd.target;
import dmd.templateparamsem;
import dmd.typesem;
import dmd.visitor;

enum LOG = false;


/*************************************
 * Does semantic analysis on initializers and members of aggregates.
 */
void semantic2(Dsymbol dsym, Scope* sc)
{
    scope v = new Semantic2Visitor(sc);
    dsym.accept(v);
}

private extern(C++) final class Semantic2Visitor : Visitor
{
    alias visit = Visitor.visit;
    Scope* sc;
    this(Scope* sc) scope @safe
    {
        this.sc = sc;
    }

    override void visit(Dsymbol) {}

    override void visit(StaticAssert sa)
    {
        //printf("StaticAssert::semantic2() %s\n", sa.toChars());
        if (const e = sa.exp.isStringExp())
        {
            // deprecated in 2.107
            deprecation(e.loc, "static assert condition cannot be a string literal");
            deprecationSupplemental(e.loc, "If intentional, use `%s !is null` instead to preserve behaviour",
                e.toChars());
        }
        auto sds = new ScopeDsymbol();
        sc = sc.push(sds);
        sc.tinst = null;
        sc.minst = null;

        import dmd.staticcond;
        bool errors;
        bool result = evalStaticCondition(sc, sa.exp, sa.exp, errors);
        sc = sc.pop();
        if (errors)
        {
            errorSupplemental(sa.loc, "while evaluating: `static assert(%s)`", sa.exp.toChars());
            return;
        }
        else if (result)
            return;

        staticAssertFail(sa, sc);
    }

    override void visit(TemplateInstance tempinst)
    {
        if (tempinst.semanticRun >= PASS.semantic2)
            return;
        tempinst.semanticRun = PASS.semantic2;
        static if (LOG)
        {
            printf("+TemplateInstance.semantic2('%s')\n", tempinst.toChars());
            scope(exit) printf("-TemplateInstance.semantic2('%s')\n", tempinst.toChars());
        }
        if (tempinst.errors || !tempinst.members)
            return;

        TemplateDeclaration tempdecl = tempinst.tempdecl.isTemplateDeclaration();
        assert(tempdecl);

        sc = tempdecl._scope;
        assert(sc);
        sc = sc.push(tempinst.argsym);
        sc = sc.push(tempinst);
        sc.tinst = tempinst;
        sc.minst = tempinst.minst;

        const needGagging = (tempinst.gagged && !global.gag);
        const olderrors = global.errors;
        const oldGaggedErrors = needGagging ? global.startGagging() : -1;

        for (size_t i = 0; i < tempinst.members.length; i++)
        {
            Dsymbol s = (*tempinst.members)[i];
            static if (LOG)
            {
                printf("\tmember '%s', kind = '%s'\n", s.toChars(), s.kind());
            }
            s.semantic2(sc);
            if (tempinst.gagged && global.errors != olderrors)
                break;
        }

        if (global.errors != olderrors)
        {
            if (!tempinst.errors)
            {
                if (!tempdecl.literal)
                    .error(tempinst.loc, "%s `%s` error instantiating", tempinst.kind, tempinst.toPrettyChars);
                if (tempinst.tinst)
                    tempinst.tinst.printInstantiationTrace();
            }
            tempinst.errors = true;
        }
        if (needGagging)
            global.endGagging(oldGaggedErrors);

        sc = sc.pop();
        sc.pop();
    }

    override void visit(TemplateMixin tmix)
    {
        if (tmix.semanticRun >= PASS.semantic2)
            return;
        tmix.semanticRun = PASS.semantic2;
        static if (LOG)
        {
            printf("+TemplateMixin.semantic2('%s')\n", tmix.toChars());
            scope(exit) printf("-TemplateMixin.semantic2('%s')\n", tmix.toChars());
        }
        if (!tmix.members)
            return;

        assert(sc);
        sc = sc.push(tmix.argsym);
        sc = sc.push(tmix);
        sc.tinst = tmix;
        sc.minst = tmix.minst;
        for (size_t i = 0; i < tmix.members.length; i++)
        {
            Dsymbol s = (*tmix.members)[i];
            static if (LOG)
            {
                printf("\tmember '%s', kind = '%s'\n", s.toChars(), s.kind());
            }
            s.semantic2(sc);
        }
        sc = sc.pop();
        sc.pop();
    }

    override void visit(VarDeclaration vd)
    {
        if (vd.semanticRun < PASS.semanticdone && vd.inuse)
            return;

        //printf("VarDeclaration::semantic2('%s')\n", toChars());
        sc = sc.push();
        sc.varDecl = vd;
        scope(exit) sc = sc.pop();

        if (vd.aliasTuple)        // if it's a tuple
        {
            vd.aliasTuple.accept(this);
            vd.semanticRun = PASS.semantic2done;
            return;
        }

        checkGNUABITag(vd, vd._linkage);

        if (vd._init && !vd.toParent().isFuncDeclaration())
        {
            vd.inuse++;

            /* https://issues.dlang.org/show_bug.cgi?id=20280
             *
             * Template instances may import modules that have not
             * finished semantic1.
             */
            if (!vd.type)
                vd.dsymbolSemantic(sc);


            // https://issues.dlang.org/show_bug.cgi?id=14166
            // https://issues.dlang.org/show_bug.cgi?id=20417
            // Don't run CTFE for the temporary variables inside typeof or __traits(compiles)
            vd._init = vd._init.initializerSemantic(sc, vd.type, sc.intypeof == 1 || sc.traitsCompiles ? INITnointerpret : INITinterpret);
            lowerStaticAAs(vd, sc);
            vd.inuse--;
        }
        if (vd._init && vd.storage_class & STC.manifest)
        {
            /* Cannot initializer enums with CTFE classreferences and addresses of struct literals.
             * Scan initializer looking for them. Issue error if found.
             */
            if (ExpInitializer ei = vd._init.isExpInitializer())
            {
                static bool hasInvalidEnumInitializer(Expression e)
                {
                    static bool arrayHasInvalidEnumInitializer(Expressions* elems)
                    {
                        foreach (e; *elems)
                        {
                            if (e && hasInvalidEnumInitializer(e))
                                return true;
                        }
                        return false;
                    }

                    if (e.op == EXP.classReference)
                        return true;
                    if (e.op == EXP.address && (cast(AddrExp)e).e1.op == EXP.structLiteral)
                        return true;
                    if (e.op == EXP.arrayLiteral)
                        return arrayHasInvalidEnumInitializer((cast(ArrayLiteralExp)e).elements);
                    if (e.op == EXP.structLiteral)
                        return arrayHasInvalidEnumInitializer((cast(StructLiteralExp)e).elements);
                    if (e.op == EXP.assocArrayLiteral)
                    {
                        auto ae = cast(AssocArrayLiteralExp)e;
                        return arrayHasInvalidEnumInitializer(ae.values) ||
                               arrayHasInvalidEnumInitializer(ae.keys);
                    }
                    return false;
                }

                if (hasInvalidEnumInitializer(ei.exp))
                    .error(vd.loc, "%s `%s` : Unable to initialize enum with class or pointer to struct. Use static const variable instead.", vd.kind, vd.toPrettyChars);
            }
        }
        else if (vd._init && vd.isThreadlocal())
        {
            // Cannot initialize a thread-local class or pointer to struct variable with a literal
            // that itself is a thread-local reference and would need dynamic initialization also.
            if (vd.type.ty == Tclass && vd.type.isMutable() && !vd.type.isShared())
            {
                ExpInitializer ei = vd._init.isExpInitializer();
                if (ei && ei.exp.op == EXP.classReference)
                    .error(vd.loc, "%s `%s` is a thread-local class and cannot have a static initializer. Use `static this()` to initialize instead.", vd.kind, vd.toPrettyChars);
            }
            else if (vd.type.ty == Tpointer && vd.type.nextOf().ty == Tstruct && vd.type.nextOf().isMutable() && !vd.type.nextOf().isShared())
            {
                ExpInitializer ei = vd._init.isExpInitializer();
                if (ei && ei.exp.op == EXP.address && (cast(AddrExp)ei.exp).e1.op == EXP.structLiteral)
                    .error(vd.loc, "%s `%s` is a thread-local pointer to struct and cannot have a static initializer. Use `static this()` to initialize instead.", vd.kind, vd.toPrettyChars);
            }
        }
        vd.semanticRun = PASS.semantic2done;
    }

    override void visit(Module mod)
    {
        //printf("Module::semantic2('%s'): parent = %p\n", toChars(), parent);
        if (mod.semanticRun != PASS.semanticdone) // semantic() not completed yet - could be recursive call
            return;
        mod.semanticRun = PASS.semantic2;
        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope* sc = Scope.createGlobal(mod, global.errorSink); // create root scope
        //printf("Module = %p\n", sc.scopesym);
        if (mod.members)
        {
            // Pass 2 semantic routines: do initializers and function bodies
            for (size_t i = 0; i < mod.members.length; i++)
            {
                Dsymbol s = (*mod.members)[i];
                s.semantic2(sc);
            }
        }
        if (mod.userAttribDecl)
        {
            mod.userAttribDecl.semantic2(sc);
        }
        sc = sc.pop();
        sc.pop();
        mod.semanticRun = PASS.semantic2done;
        //printf("-Module::semantic2('%s'): parent = %p\n", toChars(), parent);
    }

    override void visit(FuncDeclaration fd)
    {
        if (fd.semanticRun >= PASS.semantic2done)
            return;

        if (fd.semanticRun < PASS.semanticdone && !fd.errors)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=21614
             *
             * Template instances may import modules that have not
             * finished semantic1.
             */
            fd.dsymbolSemantic(sc);
        }
        assert(fd.semanticRun <= PASS.semantic2);
        fd.semanticRun = PASS.semantic2;

        timeTraceBeginEvent(TimeTraceEventType.sema2);
        scope(exit) timeTraceEndEvent(TimeTraceEventType.sema2, fd);

        //printf("FuncDeclaration::semantic2 [%s] fd: %s type: %s\n", fd.loc.toChars(), fd.toChars(), fd.type ? fd.type.toChars() : "".ptr);

        // Only check valid functions which have a body to avoid errors
        // for multiple declarations, e.g.
        // void foo();
        // void foo();
        if (fd.fbody && fd.overnext && !fd.errors)
        {
            // Always starts the lookup from 'this', because the conflicts with
            // previous overloads are already reported.
            alias f1 = fd;
            auto tf1 = cast(TypeFunction) f1.type;
            auto parent1 = f1.toParent2();
            const linkage1 = f1.resolvedLinkage();

            overloadApply(f1, (Dsymbol s)
            {
                auto f2 = s.isFuncDeclaration();
                if (!f2 || f1 == f2 || f2.errors)
                    return 0;

                // Don't have to check conflict between declaration and definition.
                if (f2.fbody is null)
                    return 0;

                // Functions with different manglings can never conflict
                if (linkage1 != f2.resolvedLinkage())
                    return 0;

                // Functions with different names never conflict
                // (they can form overloads sets introduced by an alias)
                if (f1.ident != f2.ident)
                    return 0;

                // Functions with different parents never conflict
                // (E.g. when aliasing a free function into a struct)
                if (parent1 != f2.toParent2())
                    return 0;

                /* Check for overload merging with base class member functions.
                 *
                 *  class B { void foo() {} }
                 *  class D : B {
                 *    override void foo() {}    // B.foo appears as f2
                 *    alias foo = B.foo;
                 *  }
                 */
                if (f1.overrides(f2))
                    return 0;

                auto tf2 = cast(TypeFunction) f2.type;

                // Overloading based on storage classes
                if (tf1.mod != tf2.mod || ((f1.storage_class ^ f2.storage_class) & STC.static_))
                    return 0;

                // @@@DEPRECATED_2.112@@@
                // This test doesn't catch identical functions that differ only
                // in explicit/implicit `@system` - a deprecation has now been
                // added below, remove `false` after deprecation period is over.
                const sameAttr = tf1.attributesEqual(tf2, false);
                const sameParams = tf1.parameterList == tf2.parameterList;

                // Allow the hack to declare overloads with different parameters/STC's
                if (parent1.isModule() &&
                    linkage1 != LINK.d && linkage1 != LINK.cpp &&
                    (!sameAttr || !sameParams)
                )
                {
                    .error(f2.loc, "%s `%s` cannot overload `extern(%s)` function at %s", f2.kind, f2.toPrettyChars,
                            linkageToChars(f1._linkage),
                            f1.loc.toChars());
                    return 0;
                }

                // Different parameters don't conflict in extern(C++/D)
                if (!sameParams)
                    return 0;

                // Different attributes don't conflict in extern(D)
                if (!sameAttr && linkage1 == LINK.d)
                {
                    // @@@DEPRECATED_2.112@@@
                    // Same as 2.104 deprecation, but also catching explicit/implicit `@system`
                    // At the end of deprecation period, fix Type.attributesEqual and remove
                    // this condition, as well as the error for extern(C) functions above.
                    if (sameAttr != tf1.attributesEqual(tf2))
                    {
                        .deprecation(f2.loc, "%s `%s` cannot overload `extern(%s)` function at %s", f2.kind, f2.toPrettyChars,
                                linkageToChars(f1._linkage),
                                f1.loc.toChars());
                    }
                    return 0;
                }

                .error(f2.loc, "%s `%s%s` conflicts with previous declaration at %s",
                        f2.kind(),
                        f2.toPrettyChars(),
                        parametersTypeToChars(tf2.parameterList),
                        f1.loc.toChars());
                f2.type = Type.terror;
                f2.errors = true;
                return 0;
            });
        }
        if (!fd.type || fd.type.ty != Tfunction)
            return;
        TypeFunction f = cast(TypeFunction) fd.type;

        checkGNUABITag(fd, fd._linkage);
        //semantic for parameters' UDAs
        foreach (i, param; f.parameterList)
        {
            if (param && param.userAttribDecl)
                param.userAttribDecl.semantic2(sc);
        }
    }

    override void visit(Import i)
    {
        //printf("Import::semantic2('%s')\n", toChars());
        if (!i.mod)
            return;

        i.mod.semantic2(null);
        if (i.mod.needmoduleinfo)
        {
            //printf("module5 %s because of %s\n", sc._module.toChars(), mod.toChars());
            if (sc)
                sc._module.needmoduleinfo = 1;
        }
    }

    override void visit(Nspace ns)
    {
        if (ns.semanticRun >= PASS.semantic2)
            return;
        ns.semanticRun = PASS.semantic2;
        static if (LOG)
        {
            printf("+Nspace::semantic2('%s')\n", ns.toChars());
            scope(exit) printf("-Nspace::semantic2('%s')\n", ns.toChars());
        }
        checkGNUABITag(ns, LINK.cpp);
        if (!ns.members)
            return;

        assert(sc);
        sc = sc.push(ns);
        sc.linkage = LINK.cpp;
        foreach (s; *ns.members)
        {
            static if (LOG)
            {
                printf("\tmember '%s', kind = '%s'\n", s.toChars(), s.kind());
            }
            s.semantic2(sc);
        }
        sc.pop();
    }

    override void visit(AttribDeclaration ad)
    {
        Dsymbols* d = ad.include(sc);
        if (!d)
            return;

        Scope* sc2 = ad.newScope(sc);
        for (size_t i = 0; i < d.length; i++)
        {
            Dsymbol s = (*d)[i];
            s.semantic2(sc2);
        }
        if (sc2 != sc)
            sc2.pop();
    }

    /**
     * Run the DeprecatedDeclaration's semantic2 phase then its members.
     *
     * The message set via a `DeprecatedDeclaration` can be either of:
     * - a string literal
     * - an enum
     * - a static immutable
     * So we need to call ctfe to resolve it.
     * Afterward forwards to the members' semantic2.
     */
    override void visit(DeprecatedDeclaration dd)
    {
        getMessage(dd);
        visit(cast(AttribDeclaration)dd);
    }

    override void visit(AlignDeclaration ad)
    {
        ad.getAlignment(sc);
        visit(cast(AttribDeclaration)ad);
    }

    override void visit(CPPNamespaceDeclaration decl)
    {
        checkGNUABITag(decl, LINK.cpp);
        visit(cast(AttribDeclaration)decl);
    }

    override void visit(UserAttributeDeclaration uad)
    {
        if (!uad.decl || !uad.atts || !uad.atts.length || !uad._scope)
            return visit(cast(AttribDeclaration)uad);

        Expression* lastTag;
        static void eval(Scope* sc, Expressions* exps, ref Expression* lastTag)
        {
            foreach (ref Expression e; *exps)
            {
                if (!e)
                    continue;

                e = e.expressionSemantic(sc);
                if (definitelyValueParameter(e))
                    e = e.ctfeInterpret();
                if (e.op == EXP.tuple)
                {
                    TupleExp te = cast(TupleExp)e;
                    eval(sc, te.exps, lastTag);
                }

                // Handles compiler-recognized `core.attribute.gnuAbiTag`
                if (isGNUABITag(e))
                    doGNUABITagSemantic(e, lastTag);
            }
        }

        uad._scope = null;
        eval(sc, uad.atts, lastTag);
        visit(cast(AttribDeclaration)uad);
    }

    override void visit(AggregateDeclaration ad)
    {
        //printf("AggregateDeclaration::semantic2(%s) type = %s, errors = %d\n", ad.toChars(), ad.type.toChars(), ad.errors);
        if (!ad.members)
            return;

        if (ad._scope)
        {
            .error(ad.loc, "%s `%s` has forward references", ad.kind, ad.toPrettyChars);
            return;
        }

        checkGNUABITag(ad, ad.classKind == ClassKind.cpp ? LINK.cpp : LINK.d);

        auto sc2 = ad.newScope(sc);

        ad.determineSize(ad.loc);

        for (size_t i = 0; i < ad.members.length; i++)
        {
            Dsymbol s = (*ad.members)[i];
            //printf("\t[%d] %s\n", i, s.toChars());
            s.semantic2(sc2);
        }

        sc2.pop();
    }

    override void visit(ClassDeclaration cd)
    {
        /// Checks that the given class implements all methods of its interfaces.
        static void checkInterfaceImplementations(ClassDeclaration cd)
        {
            foreach (base; cd.interfaces)
            {
                // https://issues.dlang.org/show_bug.cgi?id=22729
                // interfaces that have errors or that
                // inherit from interfaces that have errors
                // might have an uninitialized vtable
                if (!base.sym.vtbl.length)
                    continue;

                // first entry is ClassInfo reference
                auto methods = base.sym.vtbl[base.sym.vtblOffset .. $];

                foreach (m; methods)
                {
                    auto ifd = m.isFuncDeclaration;
                    assert(ifd);

                    if (ifd.objc.isOptional)
                        continue;

                    auto type = ifd.type.toTypeFunction();
                    auto fd = cd.findFunc(ifd.ident, type);

                    if (fd && !fd.isAbstract)
                    {
                        //printf("            found\n");
                        // Check that calling conventions match
                        if (fd._linkage != ifd._linkage)
                            .error(fd.loc, "%s `%s` linkage doesn't match interface function", fd.kind, fd.toPrettyChars);

                        // Check that it is current
                        //printf("newinstance = %d fd.toParent() = %s ifd.toParent() = %s\n",
                            //newinstance, fd.toParent().toChars(), ifd.toParent().toChars());
                        if (fd.toParent() != cd && ifd.toParent() == base.sym)
                            .error(cd.loc, "%s `%s` interface function `%s` is not implemented", cd.kind, cd.toPrettyChars, ifd.toFullSignature());
                    }
                    else
                    {
                        //printf("            not found %p\n", fd);
                        // BUG: should mark this class as abstract?
                        if (!cd.isAbstract())
                            .error(cd.loc, "%s `%s` interface function `%s` is not implemented", cd.kind, cd.toPrettyChars, ifd.toFullSignature());
                    }
                }
            }
        }

        if (cd.semanticRun >= PASS.semantic2done)
            return;
        assert(cd.semanticRun <= PASS.semantic2);
        cd.semanticRun = PASS.semantic2;

        checkInterfaceImplementations(cd);
        visit(cast(AggregateDeclaration) cd);
    }

    override void visit(InterfaceDeclaration cd)
    {
        visit(cast(AggregateDeclaration) cd);
    }

    override void visit(TupleDeclaration td)
    {
        td.foreachVar((s) { s.accept(this); });
    }
}

/**
 * Perform semantic analysis specific to the GNU ABI tags
 *
 * The GNU ABI tags are a feature introduced in C++11, specific to g++
 * and the Itanium ABI.
 * They are mandatory for C++ interfacing, simply because the templated struct
 *`std::basic_string`, of which the ubiquitous `std::string` is a instantiation
 * of, uses them.
 *
 * Params:
 *   e = Expression to perform semantic on
 *       See `Semantic2Visitor.visit(UserAttributeDeclaration)`
 *   lastTag = When `!is null`, we already saw an ABI tag.
 *            To simplify implementation and reflection code,
 *            only one ABI tag object is allowed per symbol
 *            (but it can have multiple tags as it's an array exp).
 */
private void doGNUABITagSemantic(ref Expression e, ref Expression* lastTag)
{
    import dmd.mangle : isValidMangling;
    // When `@gnuAbiTag` is used, the type will be the UDA, not the struct literal
    if (e.op == EXP.type)
    {
        error(e.loc, "`@%s` at least one argument expected", Id.udaGNUAbiTag.toChars());
        return;
    }

    // Definition is in `core.attributes`. If it's not a struct literal,
    // it shouldn't have passed semantic, hence the `assert`.
    auto sle = e.isStructLiteralExp();
    if (sle is null)
    {
        assert(global.errors);
        return;
    }
    // The definition of `gnuAttributes` only have 1 member, `string[] tags`
    assert(sle.elements && sle.elements.length == 1);
    // `gnuAbiTag`'s constructor is defined as `this(string[] tags...)`
    auto ale = (*sle.elements)[0].isArrayLiteralExp();
    if (ale is null)
    {
        error(e.loc, "`@%s` at least one argument expected", Id.udaGNUAbiTag.toChars());
        return;
    }

    // Check that it's the only tag on the symbol
    if (lastTag !is null)
    {
        const str1 = (*lastTag.isStructLiteralExp().elements)[0].toString();
        const str2 = ale.toString();
        error(e.loc, "only one `@%s` allowed per symbol", Id.udaGNUAbiTag.toChars());
        errorSupplemental(e.loc, "instead of `@%s @%s`, use `@%s(%.*s, %.*s)`",
            lastTag.toChars(), e.toChars(), Id.udaGNUAbiTag.toChars(),
            // Avoid [ ... ]
            cast(int)str1.length - 2, str1.ptr + 1,
            cast(int)str2.length - 2, str2.ptr + 1);
        return;
    }
    lastTag = &e;

    // We already know we have a valid array literal of strings.
    // Now checks that elements are valid.
    foreach (idx, elem; *ale.elements)
    {
        const str = elem.toStringExp().peekString();
        if (!str.length)
        {
            error(e.loc, "argument `%d` to `@%s` cannot be %s", cast(int)(idx + 1),
                    Id.udaGNUAbiTag.toChars(),
                    elem.isNullExp() ? "`null`".ptr : "empty".ptr);
            continue;
        }

        foreach (c; str)
        {
            if (!c.isValidMangling())
            {
                error(e.loc, "`@%s` char `0x%02x` not allowed in mangling",
                        Id.udaGNUAbiTag.toChars(), c);
                break;
            }
        }
        // Valid element
    }
    // Since ABI tags need to be sorted, we sort them in place
    // It might be surprising for users that inspects the UDAs,
    // but it's a concession to practicality.
    // Casts are unfortunately necessary as `implicitConvTo` is not
    // `const` (and nor is `StringExp`, by extension).
    static int predicate(const scope Expression* e1, const scope Expression* e2)
    {
        return (cast(Expression*)e1).toStringExp().compare((cast(Expression*)e2).toStringExp());
    }
    ale.elements.sort!predicate;
}

/**
 * Try lower a variable's Associative Array initializer to a newaa struct
 * so it can be put in static data.
 * Params:
 *   vd = Variable to lower
 *   sc = Scope
 */
void lowerStaticAAs(VarDeclaration vd, Scope* sc)
{
    if (vd.storage_class & STC.manifest)
        return;
    if (auto ei = vd._init.isExpInitializer())
        lowerStaticAAs(ei.exp, sc);
}

/**
 * Try lower all Associative Array literals in an expression to a newaa struct
 * so it can be put in static data.
 * Params:
 *   e = Expression to traverse
 *   sc = Scope
 */
void lowerStaticAAs(Expression e, Scope* sc)
{
    scope v = new StaticAAVisitor(sc);
    e.accept(v);
}

/// Visit Associative Array literals and lower them to structs for static initialization
private extern(C++) final class StaticAAVisitor : SemanticTimeTransitiveVisitor
{
    alias visit = SemanticTimeTransitiveVisitor.visit;
    Scope* sc;

    this(Scope* sc) scope @safe
    {
        this.sc = sc;
    }

    override void visit(AssocArrayLiteralExp aaExp)
    {
        if (!verifyHookExist(aaExp.loc, *sc, Id._aaAsStruct, "initializing static associative arrays", Id.object))
            return;

        Expression hookFunc = new IdentifierExp(aaExp.loc, Id.empty);
        hookFunc = new DotIdExp(aaExp.loc, hookFunc, Id.object);
        hookFunc = new DotIdExp(aaExp.loc, hookFunc, Id._aaAsStruct);
        auto arguments = new Expressions();
        arguments.push(aaExp);
        Expression loweredExp = new CallExp(aaExp.loc, hookFunc, arguments);

        sc = sc.startCTFE();
        loweredExp = loweredExp.expressionSemantic(sc);
        loweredExp = resolveProperties(sc, loweredExp);
        sc = sc.endCTFE();
        loweredExp = loweredExp.ctfeInterpret();

        aaExp.lowering = loweredExp;

        semanticTypeInfo(sc, loweredExp.type);
    }

    // https://issues.dlang.org/show_bug.cgi?id=24602
    // TODO: Is this intionally not visited by SemanticTimeTransitiveVisitor?
    override void visit(ClassReferenceExp crExp)
    {
        this.visit(crExp.value);
    }
}

/**
 * Given a static assert with a failing condition, print an error
 * Params:
 *   sa = Static assert with failing condition
 *   sc = scope for evaluating assert message and printing context
 */
void staticAssertFail(StaticAssert sa, Scope* sc)
{
    if (sa.msgs)
    {
        OutBuffer msgbuf;
        for (size_t i = 0; i < sa.msgs.length; i++)
        {
            Expression e = (*sa.msgs)[i];
            sc = sc.startCTFE();
            e = e.expressionSemantic(sc);
            e = resolveProperties(sc, e);
            sc = sc.endCTFE();
            e = ctfeInterpretForPragmaMsg(e);
            if (e.op == EXP.error)
            {
                errorSupplemental(sa.loc, "while evaluating `static assert` argument `%s`", (*sa.msgs)[i].toChars());
                if (!global.gag)
                    fatal();
                return;
            }
            if (StringExp se = e.toStringExp())
            {
                const slice = se.toUTF8(sc).peekString();
                // Hack to keep old formatting to avoid changing error messages everywhere
                if (sa.msgs.length == 1)
                    msgbuf.printf("\"%.*s\"", cast(int)slice.length, slice.ptr);
                else
                    msgbuf.printf("%.*s", cast(int)slice.length, slice.ptr);
            }
            else
                msgbuf.printf("%s", e.toChars());
        }
        error(sa.loc, "static assert:  %s", msgbuf.extractChars());
    }
    else
        error(sa.loc, "static assert:  `%s` is false", sa.exp.toChars());
    if (sc.tinst)
        sc.tinst.printInstantiationTrace();
    if (!global.gag)
        fatal();
}
