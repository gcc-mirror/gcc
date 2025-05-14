/**
 * Does semantic analysis for functions.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html, Functions)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/funcsem.d, _funcsem.d)
 * Documentation:  https://dlang.org/phobos/dmd_funcsem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/funcsem.d
 */

module dmd.funcsem;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.blockexit;
import dmd.gluelayer;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.delegatize;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.escape;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.importc;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.mustuse;
import dmd.objc;
import dmd.opover;
import dmd.pragmasem;
import dmd.root.aav;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.filename;
import dmd.root.string;
import dmd.root.stringtable;
import dmd.safe;
import dmd.semantic2;
import dmd.semantic3;
import dmd.statement;
import dmd.statementsem;
import dmd.target;
import dmd.templatesem;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;
import dmd.visitor.statement_rewrite_walker;

version (IN_GCC) {}
else version (IN_LLVM) {}
else version = MARS;

/* Tweak all return statements and dtor call for nrvo_var, for correct NRVO.
 */
extern (C++) final class NrvoWalker : StatementRewriteWalker
{
    alias visit = typeof(super).visit;
public:
    FuncDeclaration fd;
    Scope* sc;

    override void visit(ReturnStatement s)
    {
        // See if all returns are instead to be replaced with a goto returnLabel;
        if (fd.returnLabel)
        {
            /* Rewrite:
             *  return exp;
             * as:
             *  vresult = exp; goto Lresult;
             */
            auto gs = new GotoStatement(s.loc, Id.returnLabel);
            gs.label = fd.returnLabel;

            Statement s1 = gs;
            if (s.exp)
                s1 = new CompoundStatement(s.loc, new ExpStatement(s.loc, s.exp), gs);

            replaceCurrent(s1);
        }
    }

    override void visit(TryFinallyStatement s)
    {
        DtorExpStatement des;
        if (fd.isNRVO() && s.finalbody && (des = s.finalbody.isDtorExpStatement()) !is null &&
            fd.nrvo_var == des.var)
        {
            if (!(global.params.useExceptions && ClassDeclaration.throwable))
            {
                /* Don't need to call destructor at all, since it is nrvo
                 */
                replaceCurrent(s._body);
                s._body.accept(this);
                return;
            }

            /* Normally local variable dtors are called regardless exceptions.
             * But for nrvo_var, its dtor should be called only when exception is thrown.
             *
             * Rewrite:
             *      try { s.body; } finally { nrvo_var.edtor; }
             *      // equivalent with:
             *      //    s.body; scope(exit) nrvo_var.edtor;
             * as:
             *      try { s.body; } catch(Throwable __o) { nrvo_var.edtor; throw __o; }
             *      // equivalent with:
             *      //    s.body; scope(failure) nrvo_var.edtor;
             */
            Statement sexception = new DtorExpStatement(Loc.initial, fd.nrvo_var.edtor, fd.nrvo_var);
            Identifier id = Identifier.generateId("__o");

            Statement handler = new PeelStatement(sexception);
            if (sexception.blockExit(fd, null) & BE.fallthru)
            {
                auto ts = new ThrowStatement(Loc.initial, new IdentifierExp(Loc.initial, id));
                ts.internalThrow = true;
                handler = new CompoundStatement(Loc.initial, handler, ts);
            }

            auto catches = new Catches();
            auto ctch = new Catch(Loc.initial, getThrowable(), id, handler);
            ctch.internalCatch = true;
            ctch.catchSemantic(sc); // Run semantic to resolve identifier '__o'
            catches.push(ctch);

            Statement s2 = new TryCatchStatement(Loc.initial, s._body, catches);
            fd.hasNoEH = false;
            replaceCurrent(s2);
            s2.accept(this);
        }
        else
            StatementRewriteWalker.visit(s);
    }
}

/****************************************
 * Only one entry point function is allowed. Print error if more than one.
 * Params:
 *      fd = a "main" function
 * Returns:
 *      true if haven't seen "main" before
 */
extern (C++) bool onlyOneMain(FuncDeclaration fd)
{
    if (auto lastMain = FuncDeclaration.lastMain)
    {
        const format = (target.os == Target.OS.Windows)
            ? "only one entry point `main`, `WinMain` or `DllMain` is allowed"
            : "only one entry point `main` is allowed";
        error(fd.loc, format.ptr);
        errorSupplemental(lastMain.loc, "previously found `%s` here", lastMain.toFullSignature());
        return false;
    }
    FuncDeclaration.lastMain = fd;
    return true;
}

/**********************************
 * Main semantic routine for functions.
 */
void funcDeclarationSemantic(Scope* sc, FuncDeclaration funcdecl)
{
    version (none)
    {
        printf("FuncDeclaration::semantic(sc = %p, this = %p, '%s', linkage = %d)\n", sc, funcdecl, funcdecl.toPrettyChars(), sc.linkage);
        if (funcdecl.isFuncLiteralDeclaration())
            printf("\tFuncLiteralDeclaration()\n");
        printf("sc.parent = %s, parent = %s\n", sc.parent.toChars(), funcdecl.parent ? funcdecl.parent.toChars() : "");
        printf("type: %p, %s\n", funcdecl.type, funcdecl.type.toChars());
    }

    if (funcdecl.semanticRun != PASS.initial && funcdecl.isFuncLiteralDeclaration())
    {
        /* Member functions that have return types that are
         * forward references can have semantic() run more than
         * once on them.
         * See test\interface2.d, test20
         */
        return;
    }

    if (funcdecl.semanticRun >= PASS.semanticdone)
        return;
    assert(funcdecl.semanticRun <= PASS.semantic);
    funcdecl.semanticRun = PASS.semantic;

    if (funcdecl._scope)
    {
        sc = funcdecl._scope;
        funcdecl._scope = null;
    }

    if (!sc || funcdecl.errors)
        return;

    funcdecl.cppnamespace = sc.namespace;
    funcdecl.parent = sc.parent;
    Dsymbol parent = funcdecl.toParent();

    funcdecl.foverrides.setDim(0); // reset in case semantic() is being retried for this function

    funcdecl.storage_class |= sc.stc & ~STC.ref_;
    AggregateDeclaration ad = funcdecl.isThis();
    // Don't nest structs b/c of generated methods which should not access the outer scopes.
    // https://issues.dlang.org/show_bug.cgi?id=16627
    if (ad && !funcdecl.isGenerated())
    {
        funcdecl.storage_class |= ad.storage_class & (STC.TYPECTOR | STC.synchronized_);
        ad.makeNested();
    }
    if (sc.func)
        funcdecl.storage_class |= sc.func.storage_class & STC.disable;
    // Remove prefix storage classes silently.
    if ((funcdecl.storage_class & STC.TYPECTOR) && !(ad || funcdecl.isNested()))
        funcdecl.storage_class &= ~STC.TYPECTOR;

    //printf("function storage_class = x%llx, sc.stc = x%llx, %x\n", storage_class, sc.stc, Declaration.isFinal());

    if (sc.traitsCompiles)
        funcdecl.skipCodegen = true;

    funcdecl._linkage = sc.linkage;
    if (sc.inCfile && funcdecl.isFuncLiteralDeclaration())
        funcdecl._linkage = LINK.d; // so they are uniquely mangled

    if (auto fld = funcdecl.isFuncLiteralDeclaration())
    {
        if (fld.treq)
        {
            Type treq = fld.treq;
            assert(treq.nextOf().ty == Tfunction);
            if (treq.ty == Tdelegate)
                fld.tok = TOK.delegate_;
            else if (treq.isPtrToFunction())
                fld.tok = TOK.function_;
            else
                assert(0);
            funcdecl._linkage = treq.nextOf().toTypeFunction().linkage;
        }
    }

    // evaluate pragma(inline)
    if (auto pragmadecl = sc.inlining)
        funcdecl.inlining = evalPragmaInline(pragmadecl.loc, sc, pragmadecl.args);

    funcdecl.visibility = sc.visibility;
    funcdecl.userAttribDecl = sc.userAttribDecl;
    checkGNUABITag(funcdecl, funcdecl._linkage);
    checkMustUseReserved(funcdecl);

    if (!funcdecl.originalType)
        funcdecl.originalType = funcdecl.type.syntaxCopy();

    static TypeFunction getFunctionType(FuncDeclaration fd)
    {
        if (auto tf = fd.type.isTypeFunction())
            return tf;

        if (!fd.type.isTypeError())
        {
            .error(fd.loc, "%s `%s` `%s` must be a function instead of `%s`", fd.kind, fd.toPrettyChars, fd.toChars(), fd.type.toChars());
            fd.type = Type.terror;
        }
        fd.errors = true;
        return null;
    }

    if (sc.inCfile)
    {
        /* C11 allows a function to be declared with a typedef, D does not.
         */
        if (auto ti = funcdecl.type.isTypeIdentifier())
        {
            auto tj = ti.typeSemantic(funcdecl.loc, sc);
            if (auto tjf = tj.isTypeFunction())
            {
                /* Copy the type instead of just pointing to it,
                 * as we don't merge function types
                 */
                auto tjf2 = new TypeFunction(tjf.parameterList, tjf.next, tjf.linkage);
                funcdecl.type = tjf2;
                funcdecl.originalType = tjf2;
            }
        }
    }

    if (!getFunctionType(funcdecl))
        return;

    if (!funcdecl.type.deco)
    {
        sc = sc.push();
        sc.stc |= funcdecl.storage_class & (STC.disable | STC.deprecated_); // forward to function type

        TypeFunction tf = funcdecl.type.toTypeFunction();
        if (sc.func)
        {
            /* If the nesting parent is pure without inference,
             * then this function defaults to pure too.
             *
             *  auto foo() pure {
             *    auto bar() {}     // become a weak purity function
             *    class C {         // nested class
             *      auto baz() {}   // become a weak purity function
             *    }
             *
             *    static auto boo() {}   // typed as impure
             *    // Even though, boo cannot call any impure functions.
             *    // See also Expression::checkPurity().
             *  }
             */
            if (tf.purity == PURE.impure && (funcdecl.isNested() || funcdecl.isThis()))
            {
                FuncDeclaration fd = null;
                for (Dsymbol p = funcdecl.toParent2(); p; p = p.toParent2())
                {
                    if (AggregateDeclaration adx = p.isAggregateDeclaration())
                    {
                        if (adx.isNested())
                            continue;
                        break;
                    }
                    if ((fd = p.isFuncDeclaration()) !is null)
                        break;
                }

                /* If the parent's purity is inferred, then this function's purity needs
                 * to be inferred first.
                 */
                if (fd && fd.isPureBypassingInference() >= PURE.weak && !funcdecl.isInstantiated())
                {
                    tf.purity = PURE.fwdref; // default to pure
                }
            }
        }

        if (tf.isRef)
            sc.stc |= STC.ref_;
        if (tf.isScopeQual)
            sc.stc |= STC.scope_;
        if (tf.isNothrow)
            sc.stc |= STC.nothrow_;
        if (tf.isNogc)
            sc.stc |= STC.nogc;
        if (tf.isProperty)
            sc.stc |= STC.property;
        if (tf.purity == PURE.fwdref)
            sc.stc |= STC.pure_;

        if (tf.trust != TRUST.default_)
        {
            sc.stc &= ~STC.safeGroup;
            if (tf.trust == TRUST.safe)
                sc.stc |= STC.safe;
            else if (tf.trust == TRUST.system)
                sc.stc |= STC.system;
            else if (tf.trust == TRUST.trusted)
                sc.stc |= STC.trusted;
        }

        if (funcdecl.isCtorDeclaration())
        {
            tf.isCtor = true;
            Type tret = ad.handleType();
            assert(tret);
            tret = tret.addStorageClass(funcdecl.storage_class | sc.stc);
            tret = tret.addMod(funcdecl.type.mod);
            tf.next = tret;
            if (ad.isStructDeclaration())
                sc.stc |= STC.ref_;
        }

        // 'return' on a non-static class member function implies 'scope' as well
        if (ad && ad.isClassDeclaration() && (tf.isReturn || sc.stc & STC.return_) && !(sc.stc & STC.static_))
            sc.stc |= STC.scope_;

        // If 'this' has no pointers, remove 'scope' as it has no meaning
        // Note: this is already covered by semantic of `VarDeclaration` and `TypeFunction`,
        // but existing code relies on `hasPointers()` being called here to resolve forward references:
        // https://github.com/dlang/dmd/pull/14232#issuecomment-1162906573
        if (sc.stc & STC.scope_ && ad && ad.isStructDeclaration() && !ad.type.hasPointers())
        {
            sc.stc &= ~STC.scope_;
            tf.isScopeQual = false;
            if (tf.isReturnScope)
            {
                sc.stc &= ~(STC.return_ | STC.returnScope);
                tf.isReturn = false;
                tf.isReturnScope = false;
            }
        }

        sc.linkage = funcdecl._linkage;

        if (!tf.isNaked() && !(funcdecl.isThis() || funcdecl.isNested()))
        {
            import core.bitop : popcnt;
            auto mods = MODtoChars(tf.mod);
            .error(funcdecl.loc, "%s `%s` without `this` cannot be `%s`", funcdecl.kind, funcdecl.toPrettyChars, mods);
            if (tf.next && tf.next.ty != Tvoid && popcnt(tf.mod) == 1)
                .errorSupplemental(funcdecl.loc,
                    "did you mean to use `%s(%s)` as the return type?", mods, tf.next.toChars());

            tf.mod = 0; // remove qualifiers
        }

        /* Apply const, immutable, wild and shared storage class
         * to the function type. Do this before type semantic.
         */
        auto stc = funcdecl.storage_class;
        if (funcdecl.type.isImmutable())
            stc |= STC.immutable_;
        if (funcdecl.type.isConst())
            stc |= STC.const_;
        if (funcdecl.type.isShared() || funcdecl.storage_class & STC.synchronized_)
            stc |= STC.shared_;
        if (funcdecl.type.isWild())
            stc |= STC.wild;
        funcdecl.type = funcdecl.type.addSTC(stc);

        funcdecl.type = funcdecl.type.typeSemantic(funcdecl.loc, sc);
        sc = sc.pop();
    }

    auto f = getFunctionType(funcdecl);
    if (!f)
        return;     // funcdecl's type is not a function

    {
        // Merge back function attributes into 'originalType'.
        // It's used for mangling, ddoc, and json output.
        TypeFunction tfo = funcdecl.originalType.toTypeFunction();
        tfo.mod = f.mod;
        tfo.isScopeQual = f.isScopeQual;
        tfo.isReturnInferred = f.isReturnInferred;
        tfo.isScopeInferred = f.isScopeInferred;
        tfo.isRef = f.isRef;
        tfo.isNothrow = f.isNothrow;
        tfo.isNogc = f.isNogc;
        tfo.isProperty = f.isProperty;
        tfo.purity = f.purity;
        tfo.trust = f.trust;

        funcdecl.storage_class &= ~(STC.TYPECTOR | STC.FUNCATTR);
    }

    // check pragma(crt_constructor) signature
    if (funcdecl.isCrtCtor || funcdecl.isCrtDtor)
    {
        const idStr = funcdecl.isCrtCtor ? "crt_constructor" : "crt_destructor";
        if (f.nextOf().ty != Tvoid)
            .error(funcdecl.loc, "%s `%s` must return `void` for `pragma(%s)`", funcdecl.kind, funcdecl.toPrettyChars, idStr.ptr);
        if (funcdecl._linkage != LINK.c && f.parameterList.length != 0)
            .error(funcdecl.loc, "%s `%s` must be `extern(C)` for `pragma(%s)` when taking parameters", funcdecl.kind, funcdecl.toPrettyChars, idStr.ptr);
        if (funcdecl.isThis())
            .error(funcdecl.loc, "%s `%s` cannot be a non-static member function for `pragma(%s)`", funcdecl.kind, funcdecl.toPrettyChars, idStr.ptr);
    }

    if (funcdecl.overnext && funcdecl.isCsymbol())
    {
        /* C does not allow function overloading, but it does allow
         * redeclarations of the same function. If .overnext points
         * to a redeclaration, ok. Error if it is an overload.
         */
        auto fnext = funcdecl.overnext.isFuncDeclaration();
        funcDeclarationSemantic(sc, fnext);
        auto fn = fnext.type.isTypeFunction();
        if (!fn || !cFuncEquivalence(f, fn))
        {
            .error(funcdecl.loc, "%s `%s` redeclaration with different type", funcdecl.kind, funcdecl.toPrettyChars);
            //printf("t1: %s\n", f.toChars());
            //printf("t2: %s\n", fn.toChars());
        }
        funcdecl.overnext = null;   // don't overload the redeclarations
    }

    if ((funcdecl.storage_class & STC.auto_) && !f.isRef && !funcdecl.inferRetType)
        .error(funcdecl.loc, "%s `%s` storage class `auto` has no effect if return type is not inferred", funcdecl.kind, funcdecl.toPrettyChars);

    if (f.isReturn && !funcdecl.needThis() && !funcdecl.isNested())
    {
        /* Non-static nested functions have a hidden 'this' pointer to which
         * the 'return' applies
         */
        if (sc.scopesym && sc.scopesym.isAggregateDeclaration())
            .error(funcdecl.loc, "%s `%s` `static` member has no `this` to which `return` can apply", funcdecl.kind, funcdecl.toPrettyChars);
        else
            error(funcdecl.loc, "top-level function `%s` has no `this` to which `return` can apply", funcdecl.toChars());
    }

    if (funcdecl.isAbstract() && !funcdecl.isVirtual())
    {
        const(char)* sfunc;
        if (funcdecl.isStatic())
            sfunc = "static";
        else if (funcdecl.visibility.kind == Visibility.Kind.private_ || funcdecl.visibility.kind == Visibility.Kind.package_)
            sfunc = visibilityToChars(funcdecl.visibility.kind);
        else
            sfunc = "final";
        .error(funcdecl.loc, "%s `%s` `%s` functions cannot be `abstract`", funcdecl.kind, funcdecl.toPrettyChars, sfunc);
    }

    if (funcdecl.isOverride() && !funcdecl.isVirtual() && !funcdecl.isFuncLiteralDeclaration())
    {
        Visibility.Kind kind = funcdecl.visible().kind;
        if ((kind == Visibility.Kind.private_ || kind == Visibility.Kind.package_) && funcdecl.isMember())
            .error(funcdecl.loc, "%s `%s` `%s` method is not virtual and cannot override", funcdecl.kind, funcdecl.toPrettyChars, visibilityToChars(kind));
        else
            .error(funcdecl.loc, "%s `%s` cannot override a non-virtual function", funcdecl.kind, funcdecl.toPrettyChars);
    }

    if (funcdecl.isAbstract() && funcdecl.isFinalFunc())
        .error(funcdecl.loc, "%s `%s` cannot be both `final` and `abstract`", funcdecl.kind, funcdecl.toPrettyChars);

    if (funcdecl.printf || funcdecl.scanf)
    {
        checkPrintfScanfSignature(funcdecl, f, sc);
    }

    if (auto id = parent.isInterfaceDeclaration())
    {
        funcdecl.storage_class |= STC.abstract_;
        if (funcdecl.isCtorDeclaration() || funcdecl.isPostBlitDeclaration() || funcdecl.isDtorDeclaration() || funcdecl.isInvariantDeclaration() || funcdecl.isNewDeclaration() || funcdecl.isDelete())
            .error(funcdecl.loc, "%s `%s` constructors, destructors, postblits, invariants, new and delete functions are not allowed in interface `%s`", funcdecl.kind, funcdecl.toPrettyChars, id.toChars());
        if (funcdecl.fbody && funcdecl.isVirtual())
            .error(funcdecl.loc, "%s `%s` function body only allowed in `final` functions in interface `%s`", funcdecl.kind, funcdecl.toPrettyChars, id.toChars());
    }

    if (UnionDeclaration ud = parent.isUnionDeclaration())
    {
        if (funcdecl.isPostBlitDeclaration() || funcdecl.isDtorDeclaration() || funcdecl.isInvariantDeclaration())
            .error(funcdecl.loc, "%s `%s` destructors, postblits and invariants are not allowed in union `%s`", funcdecl.kind, funcdecl.toPrettyChars, ud.toChars());
    }

    if (StructDeclaration sd = parent.isStructDeclaration())
    {
        if (funcdecl.isCtorDeclaration())
        {
            goto Ldone;
        }
    }

    if (ClassDeclaration cd = parent.isClassDeclaration())
    {
        parent = cd = objc.getParent(funcdecl, cd);

        if (funcdecl.isCtorDeclaration())
        {
            goto Ldone;
        }

        if (funcdecl.storage_class & STC.abstract_)
            cd.isabstract = ThreeState.yes;

        // if static function, do not put in vtbl[]
        if (!funcdecl.isVirtual())
        {
            //printf("\tnot virtual\n");
            goto Ldone;
        }
        // Suppress further errors if the return type is an error
        if (funcdecl.type.nextOf() == Type.terror)
            goto Ldone;

        bool may_override = false;
        for (size_t i = 0; i < cd.baseclasses.length; i++)
        {
            BaseClass* b = (*cd.baseclasses)[i];
            ClassDeclaration cbd = b.type.toBasetype().isClassHandle();
            if (!cbd)
                continue;
            for (size_t j = 0; j < cbd.vtbl.length; j++)
            {
                FuncDeclaration f2 = cbd.vtbl[j].isFuncDeclaration();
                if (!f2 || f2.ident != funcdecl.ident)
                    continue;
                if (cbd.parent && cbd.parent.isTemplateInstance())
                {
                    if (!functionSemantic(f2))
                        goto Ldone;
                }
                may_override = true;
            }
        }
        if (may_override && funcdecl.type.nextOf() is null)
        {
            /* If same name function exists in base class but 'this' is auto return,
             * cannot find index of base class's vtbl[] to override.
             */
            .error(funcdecl.loc, "%s `%s` return type inference is not supported if may override base class function", funcdecl.kind, funcdecl.toPrettyChars);
        }

        /* Find index of existing function in base class's vtbl[] to override
         * (the index will be the same as in cd's current vtbl[])
         */
        int vi = cd.baseClass ? findVtblIndex(funcdecl, cd.baseClass.vtbl[]) : -1;

        bool doesoverride = false;
        switch (vi)
        {
        case -1:
        Lintro:
            /* Didn't find one, so
             * This is an 'introducing' function which gets a new
             * slot in the vtbl[].
             */

            // Verify this doesn't override previous final function
            if (cd.baseClass)
            {
                Dsymbol s = cd.baseClass.search(funcdecl.loc, funcdecl.ident);
                if (s)
                {
                    if (auto f2 = s.isFuncDeclaration())
                    {
                        f2 = f2.overloadExactMatch(funcdecl.type);
                        if (f2 && f2.isFinalFunc() && f2.visible().kind != Visibility.Kind.private_)
                            .error(funcdecl.loc, "%s `%s` cannot override `final` function `%s`", funcdecl.kind, funcdecl.toPrettyChars, f2.toPrettyChars());
                    }
                }
            }

            /* These quirky conditions mimic what happens when virtual
               inheritance is implemented by producing a virtual base table
               with offsets to each of the virtual bases.
             */
            if (target.cpp.splitVBasetable && cd.classKind == ClassKind.cpp &&
                cd.baseClass && cd.baseClass.vtbl.length)
            {
                /* if overriding an interface function, then this is not
                 * introducing and don't put it in the class vtbl[]
                 */
                funcdecl.interfaceVirtual = overrideInterface(funcdecl);
                if (funcdecl.interfaceVirtual)
                {
                    //printf("\tinterface function %s\n", toChars());
                    cd.vtblFinal.push(funcdecl);
                    goto Linterfaces;
                }
            }

            if (funcdecl.isFinalFunc())
            {
                // Don't check here, as it may override an interface function
                //if (isOverride())
                //    error("is marked as override, but does not override any function");
                cd.vtblFinal.push(funcdecl);
            }
            else
            {
                //printf("\tintroducing function %s\n", funcdecl.toChars());
                funcdecl.isIntroducing = true;
                if (cd.classKind == ClassKind.cpp && target.cpp.reverseOverloads)
                {
                    /* Overloaded functions with same name are grouped and in reverse order.
                     * Search for first function of overload group, and insert
                     * funcdecl into vtbl[] immediately before it.
                     */
                    funcdecl.vtblIndex = cast(int)cd.vtbl.length;
                    bool found;
                    foreach (const i, s; cd.vtbl)
                    {
                        if (found)
                            // the rest get shifted forward
                            ++s.isFuncDeclaration().vtblIndex;
                        else if (s.ident == funcdecl.ident && s.parent == parent)
                        {
                            // found first function of overload group
                            funcdecl.vtblIndex = cast(int)i;
                            found = true;
                            ++s.isFuncDeclaration().vtblIndex;
                        }
                    }
                    cd.vtbl.insert(funcdecl.vtblIndex, funcdecl);

                    debug foreach (const i, s; cd.vtbl)
                    {
                        // a C++ dtor gets its vtblIndex later (and might even be added twice to the vtbl),
                        // e.g. when compiling druntime with a debug compiler, namely with core.stdcpp.exception.
                        if (auto fd = s.isFuncDeclaration())
                            assert(fd.vtblIndex == i ||
                                   (cd.classKind == ClassKind.cpp && fd.isDtorDeclaration) ||
                                   funcdecl.parent.isInterfaceDeclaration); // interface functions can be in multiple vtbls
                    }
                }
                else
                {
                    // Append to end of vtbl[]
                    vi = cast(int)cd.vtbl.length;
                    cd.vtbl.push(funcdecl);
                    funcdecl.vtblIndex = vi;
                }
            }
            break;

        case -2:
            // can't determine because of forward references
            funcdecl.errors = true;
            return;

        default:
            {
                if (vi >= cd.vtbl.length)
                {
                    /* the derived class cd doesn't have its vtbl[] allocated yet.
                     * https://issues.dlang.org/show_bug.cgi?id=21008
                     */
                    .error(funcdecl.loc, "%s `%s` circular reference to class `%s`", funcdecl.kind, funcdecl.toPrettyChars, cd.toChars());
                    funcdecl.errors = true;
                    return;
                }
                FuncDeclaration fdv = cd.baseClass.vtbl[vi].isFuncDeclaration();
                FuncDeclaration fdc = cd.vtbl[vi].isFuncDeclaration();
                // This function is covariant with fdv

                if (fdc == funcdecl)
                {
                    doesoverride = true;
                    break;
                }

                auto vtf = getFunctionType(fdv);
                if (vtf.trust > TRUST.system && f.trust == TRUST.system)
                    .error(funcdecl.loc, "%s `%s` cannot override `@safe` method `%s` with a `@system` attribute", funcdecl.kind, funcdecl.toPrettyChars,
                                   fdv.toPrettyChars);

                if (fdc.toParent() == parent)
                {
                    //printf("vi = %d,\tthis = %p %s %s @ [%s]\n\tfdc  = %p %s %s @ [%s]\n\tfdv  = %p %s %s @ [%s]\n",
                    //        vi, this, this.toChars(), this.type.toChars(), this.loc.toChars(),
                    //            fdc,  fdc .toChars(), fdc .type.toChars(), fdc .loc.toChars(),
                    //            fdv,  fdv .toChars(), fdv .type.toChars(), fdv .loc.toChars());

                    // fdc overrides fdv exactly, then this introduces new function.
                    if (fdc.type.mod == fdv.type.mod && funcdecl.type.mod != fdv.type.mod)
                        goto Lintro;
                }

                if (fdv.isDeprecated && !funcdecl.isDeprecated)
                    deprecation(funcdecl.loc, "`%s` is overriding the deprecated method `%s`",
                                funcdecl.toPrettyChars, fdv.toPrettyChars);

                // This function overrides fdv
                if (fdv.isFinalFunc())
                    .error(funcdecl.loc, "%s `%s` cannot override `final` function `%s`", funcdecl.kind, funcdecl.toPrettyChars, fdv.toPrettyChars());

                if (!funcdecl.isOverride())
                {
                    if (fdv.isFuture())
                    {
                        deprecation(funcdecl.loc, "method `%s` implicitly overrides `@__future` base class method; rename the former",
                            funcdecl.toPrettyChars());
                        deprecationSupplemental(fdv.loc, "base method `%s` defined here",
                            fdv.toPrettyChars());
                        // Treat 'this' as an introducing function, giving it a separate hierarchy in the vtbl[]
                        goto Lintro;
                    }
                    else
                    {
                        // https://issues.dlang.org/show_bug.cgi?id=17349
                        error(funcdecl.loc, "cannot implicitly override base class method `%s` with `%s`; add `override` attribute",
                              fdv.toPrettyChars(), funcdecl.toPrettyChars());
                    }
                }
                doesoverride = true;
                if (fdc.toParent() == parent)
                {
                    // If both are mixins, or both are not, then error.
                    // If either is not, the one that is not overrides the other.
                    bool thismixin = funcdecl.parent.isClassDeclaration() !is null;
                    bool fdcmixin = fdc.parent.isClassDeclaration() !is null;
                    if (thismixin == fdcmixin)
                    {
                        .error(funcdecl.loc, "%s `%s` multiple overrides of same function", funcdecl.kind, funcdecl.toPrettyChars);
                    }
                    /*
                     * https://issues.dlang.org/show_bug.cgi?id=711
                     *
                     * If an overriding method is introduced through a mixin,
                     * we need to update the vtbl so that both methods are
                     * present.
                     */
                    else if (thismixin)
                    {
                        /* if the mixin introduced the overriding method, then reintroduce it
                         * in the vtbl. The initial entry for the mixined method
                         * will be updated at the end of the enclosing `if` block
                         * to point to the current (non-mixined) function.
                         */
                        auto vitmp = cast(int)cd.vtbl.length;
                        cd.vtbl.push(fdc);
                        fdc.vtblIndex = vitmp;
                    }
                    else if (fdcmixin)
                    {
                        /* if the current overriding function is coming from a
                         * mixined block, then push the current function in the
                         * vtbl, but keep the previous (non-mixined) function as
                         * the overriding one.
                         */
                        auto vitmp = cast(int)cd.vtbl.length;
                        cd.vtbl.push(funcdecl);
                        funcdecl.vtblIndex = vitmp;
                        break;
                    }
                    else // fdc overrides fdv
                    {
                        // this doesn't override any function
                        break;
                    }
                }
                cd.vtbl[vi] = funcdecl;
                funcdecl.vtblIndex = vi;

                /* Remember which functions this overrides
                 */
                funcdecl.foverrides.push(fdv);

                /* This works by whenever this function is called,
                 * it actually returns tintro, which gets dynamically
                 * cast to type. But we know that tintro is a base
                 * of type, so we could optimize it by not doing a
                 * dynamic cast, but just subtracting the isBaseOf()
                 * offset if the value is != null.
                 */

                if (fdv.tintro)
                    funcdecl.tintro = fdv.tintro;
                else if (!funcdecl.type.equals(fdv.type))
                {
                    auto tnext = funcdecl.type.nextOf();
                    if (auto handle = tnext.isClassHandle())
                    {
                        if (handle.semanticRun < PASS.semanticdone && !handle.isBaseInfoComplete())
                            handle.dsymbolSemantic(null);
                    }
                    /* Only need to have a tintro if the vptr
                     * offsets differ
                     */
                    int offset;
                    if (fdv.type.nextOf().isBaseOf(tnext, &offset))
                    {
                        funcdecl.tintro = fdv.type;
                    }
                }
                break;
            }
        }

        /* Go through all the interface bases.
         * If this function is covariant with any members of those interface
         * functions, set the tintro.
         */
    Linterfaces:
        bool foundVtblMatch = false;

        for (ClassDeclaration bcd = cd; !foundVtblMatch && bcd; bcd = bcd.baseClass)
        {
            foreach (b; bcd.interfaces)
            {
                vi = findVtblIndex(funcdecl, b.sym.vtbl[]);
                switch (vi)
                {
                case -1:
                    break;

                case -2:
                    // can't determine because of forward references
                    funcdecl.errors = true;
                    return;

                default:
                    {
                        auto fdv = cast(FuncDeclaration)b.sym.vtbl[vi];
                        Type ti = null;

                        foundVtblMatch = true;

                        /* Remember which functions this overrides
                         */
                        funcdecl.foverrides.push(fdv);

                        if (fdv.tintro)
                            ti = fdv.tintro;
                        else if (!funcdecl.type.equals(fdv.type))
                        {
                            /* Only need to have a tintro if the vptr
                             * offsets differ
                             */
                            int offset;
                            if (fdv.type.nextOf().isBaseOf(funcdecl.type.nextOf(), &offset))
                            {
                                ti = fdv.type;
                            }
                        }
                        if (ti)
                        {
                            if (funcdecl.tintro)
                            {
                                if (!funcdecl.tintro.nextOf().equals(ti.nextOf()) && !funcdecl.tintro.nextOf().isBaseOf(ti.nextOf(), null) && !ti.nextOf().isBaseOf(funcdecl.tintro.nextOf(), null))
                                {
                                    .error(funcdecl.loc, "%s `%s` incompatible covariant types `%s` and `%s`", funcdecl.kind, funcdecl.toPrettyChars, funcdecl.tintro.toChars(), ti.toChars());
                                }
                            }
                            else
                            {
                                funcdecl.tintro = ti;
                            }
                        }
                    }
                }
            }
        }
        if (foundVtblMatch)
        {
            goto L2;
        }

        if (!doesoverride && funcdecl.isOverride() && (funcdecl.type.nextOf() || !may_override))
        {
            BaseClass* bc = null;
            Dsymbol s = null;
            for (size_t i = 0; i < cd.baseclasses.length; i++)
            {
                bc = (*cd.baseclasses)[i];
                s = bc.sym.search_correct(funcdecl.ident);
                if (s)
                    break;
            }

            if (s)
            {
                HdrGenState hgs;
                OutBuffer buf;

                auto fd = s.isFuncDeclaration();
                functionToBufferFull(cast(TypeFunction)(funcdecl.type), buf,
                    new Identifier(funcdecl.toPrettyChars()), hgs, null);
                const(char)* funcdeclToChars = buf.peekChars();

                if (fd)
                {
                    OutBuffer buf1;

                    if (fd.ident == funcdecl.ident)
                        hgs.fullQual = true;

                    // https://issues.dlang.org/show_bug.cgi?id=23745
                    // If the potentially overridden function contains errors,
                    // inform the user to fix that one first
                    if (fd.errors)
                    {
                        error(funcdecl.loc, "function `%s` does not override any function, did you mean to override `%s`?",
                            funcdecl.toChars(), fd.toPrettyChars());
                        errorSupplemental(fd.loc, "Function `%s` contains errors in its declaration, therefore it cannot be correctly overridden",
                            fd.toPrettyChars());
                    }
                    else
                    {
                        functionToBufferFull(cast(TypeFunction)(fd.type), buf1,
                            new Identifier(fd.toPrettyChars()), hgs, null);

                        error(funcdecl.loc, "function `%s` does not override any function, did you mean to override `%s`?",
                            funcdeclToChars, buf1.peekChars());
                   }
                }
                else
                {
                    error(funcdecl.loc, "function `%s` does not override any function, did you mean to override %s `%s`?",
                        funcdeclToChars, s.kind, s.toPrettyChars());
                    errorSupplemental(funcdecl.loc, "Functions are the only declarations that may be overridden");
                }
            }
            else
                .error(funcdecl.loc, "%s `%s` does not override any function", funcdecl.kind, funcdecl.toPrettyChars);
        }

    L2:
        objc.setSelector(funcdecl, sc);
        objc.checkLinkage(funcdecl);
        objc.addToClassMethodList(funcdecl, cd);
        objc.setAsOptional(funcdecl, sc);

        /* Go through all the interface bases.
         * Disallow overriding any final functions in the interface(s).
         */
        foreach (b; cd.interfaces)
        {
            if (b.sym)
            {
                if (auto s = search_function(b.sym, funcdecl.ident))
                {
                    if (auto f2 = s.isFuncDeclaration())
                    {
                        f2 = f2.overloadExactMatch(funcdecl.type);
                        if (f2 && f2.isFinalFunc() && f2.visible().kind != Visibility.Kind.private_)
                            .error(funcdecl.loc, "%s `%s` cannot override `final` function `%s.%s`", funcdecl.kind, funcdecl.toPrettyChars, b.sym.toChars(), f2.toPrettyChars());
                    }
                }
            }
        }

        if (funcdecl.isOverride)
        {
            if (funcdecl.storage_class & STC.disable)
                deprecation(funcdecl.loc,
                            "`%s` cannot be annotated with `@disable` because it is overriding a function in the base class",
                            funcdecl.toPrettyChars);

            if (funcdecl.isDeprecated && !(funcdecl.foverrides.length && funcdecl.foverrides[0].isDeprecated))
                deprecation(funcdecl.loc,
                            "`%s` cannot be marked as `deprecated` because it is overriding a function in the base class",
                            funcdecl.toPrettyChars);
        }

    }
    else if (funcdecl.isOverride() && !parent.isTemplateInstance())
        .error(funcdecl.loc, "%s `%s` `override` only applies to class member functions", funcdecl.kind, funcdecl.toPrettyChars);

    if (auto ti = parent.isTemplateInstance)
    {
        objc.setSelector(funcdecl, sc);
        objc.setAsOptional(funcdecl, sc);
    }

    objc.validateSelector(funcdecl);
    objc.validateOptional(funcdecl);
    // Reflect this.type to f because it could be changed by findVtblIndex
    f = funcdecl.type.toTypeFunction();

Ldone:
    if (!funcdecl.fbody && !funcdecl.allowsContractWithoutBody())
        .error(funcdecl.loc, "%s `%s` `in` and `out` contracts can only appear without a body when they are virtual interface functions or abstract", funcdecl.kind, funcdecl.toPrettyChars);

    /* Do not allow template instances to add virtual functions
     * to a class.
     */
    if (funcdecl.isVirtual())
    {
        if (auto ti = parent.isTemplateInstance())
        {
            // Take care of nested templates
            while (1)
            {
                TemplateInstance ti2 = ti.tempdecl.parent.isTemplateInstance();
                if (!ti2)
                    break;
                ti = ti2;
            }

            // If it's a member template
            if (ClassDeclaration cd = ti.tempdecl.isClassMember())
            {
                .error(funcdecl.loc, "%s `%s` cannot use template to add virtual function to class `%s`", funcdecl.kind, funcdecl.toPrettyChars, cd.toChars());
            }
        }
    }

    funcdecl.checkMain();       // Check main() parameters and return type

    /* Purity and safety can be inferred for some functions by examining
     * the function body.
     */
    if (funcdecl.canInferAttributes(sc))
        funcdecl.initInferAttributes();

    funcdecl.semanticRun = PASS.semanticdone;

    /* Save scope for possible later use (if we need the
     * function internals)
     */
    funcdecl._scope = sc.copy();
    funcdecl._scope.setNoFree();

    __gshared bool printedMain = false; // semantic might run more than once
    if (global.params.v.verbose && !printedMain)
    {
        const(char)* type = funcdecl.isMain() ? "main" : funcdecl.isWinMain() ? "winmain" : funcdecl.isDllMain() ? "dllmain" : cast(const(char)*)null;
        Module mod = sc._module;

        if (type && mod)
        {
            printedMain = true;
            auto name = mod.srcfile.toChars();
            auto path = FileName.searchPath(global.importPaths, name, true);
            message("entry     %-10s\t%s", type, path ? path : name);
        }
    }

    if (funcdecl.fbody && sc._module.isRoot() &&
        (funcdecl.isMain() || funcdecl.isWinMain() || funcdecl.isDllMain() || funcdecl.isCMain()))
        global.hasMainFunction = true;

    if (funcdecl.fbody && funcdecl.isMain() && sc._module.isRoot())
    {
        // check if `_d_cmain` is defined
        bool cmainTemplateExists()
        {
            Dsymbol pscopesym;
            auto rootSymbol = sc.search(funcdecl.loc, Id.empty, pscopesym);
            if (auto moduleSymbol = rootSymbol.search(funcdecl.loc, Id.object))
                if (moduleSymbol.search(funcdecl.loc, Id.CMain))
                    return true;

            return false;
        }

        // Only mixin `_d_cmain` if it is defined
        if (cmainTemplateExists())
        {
            // add `mixin _d_cmain!();` to the declaring module
            auto tqual = new TypeIdentifier(funcdecl.loc, Id.CMain);
            auto tm = new TemplateMixin(funcdecl.loc, null, tqual, null);
            sc._module.members.push(tm);
        }
    }

    assert(funcdecl.type.ty != Terror || funcdecl.errors);

    // semantic for parameters' UDAs
    foreach (i, param; f.parameterList)
    {
        if (param && param.userAttribDecl)
            param.userAttribDecl.dsymbolSemantic(sc);
    }
}

/*****************************************
 * Initialize for inferring the attributes of this function.
 */
private void initInferAttributes(FuncDeclaration fd)
{
    //printf("initInferAttributes() for %s (%s)\n", toPrettyChars(), ident.toChars());
    TypeFunction tf = fd.type.toTypeFunction();
    if (tf.purity == PURE.impure) // purity not specified
        fd.purityInprocess = true;

    if (tf.trust == TRUST.default_)
        fd.safetyInprocess = true;

    if (!tf.isNothrow)
        fd.nothrowInprocess = true;

    if (!tf.isNogc)
        fd.nogcInprocess = true;

    // Initialize for inferring STC.scope_
    fd.scopeInprocess = true;
}

/****************************************************
 * Resolve forward reference of function signature -
 * parameter types, return type, and attributes.
 * Params:
 *  fd = function declaration
 * Returns:
 *  false if any errors exist in the signature.
 */
public
bool functionSemantic(FuncDeclaration fd)
{
    //printf("functionSemantic() %p %s\n", this, toChars());
    if (!fd._scope)
        return !fd.errors;

    fd.cppnamespace = fd._scope.namespace;

    if (!fd.originalType) // semantic not yet run
    {
        TemplateInstance spec = fd.isSpeculative();
        const olderrs = global.errors;
        const oldgag = global.gag;
        if (global.gag && !spec)
            global.gag = 0;
        dsymbolSemantic(fd, fd._scope);
        global.gag = oldgag;
        if (spec && global.errors != olderrs)
            spec.errors = (global.errors - olderrs != 0);
        if (olderrs != global.errors) // if errors compiling this function
            return false;
    }

    // if inferring return type, sematic3 needs to be run
    // - When the function body contains any errors, we cannot assume
    //   the inferred return type is valid.
    //   So, the body errors should become the function signature error.
    if (fd.inferRetType && fd.type && !fd.type.nextOf())
        return fd.functionSemantic3();

    TemplateInstance ti;
    if (fd.isInstantiated() && !fd.isVirtualMethod() &&
        ((ti = fd.parent.isTemplateInstance()) is null || ti.isTemplateMixin() || ti.tempdecl.ident == fd.ident))
    {
        AggregateDeclaration ad = fd.isMemberLocal();
        if (ad && ad.sizeok != Sizeok.done)
        {
            /* Currently dmd cannot resolve forward references per methods,
             * then setting SIZOKfwd is too conservative and would break existing code.
             * So, just stop method attributes inference until ad.dsymbolSemantic() done.
             */
            //ad.sizeok = Sizeok.fwd;
        }
        else
            return fd.functionSemantic3() || !fd.errors;
    }

    if (fd.storage_class & STC.inference)
        return fd.functionSemantic3() || !fd.errors;

    return !fd.errors;
}

/****************************************************
 * Resolve forward reference of function body.
 * Returns false if any errors exist in the body.
 */
public
bool functionSemantic3(FuncDeclaration fd)
{
    if (fd.semanticRun < PASS.semantic3 && fd._scope)
    {
        /* Forward reference - we need to run semantic3 on this function.
         * If errors are gagged, and it's not part of a template instance,
         * we need to temporarily ungag errors.
         */
        TemplateInstance spec = fd.isSpeculative();
        const olderrs = global.errors;
        const oldgag = global.gag;
        if (global.gag && !spec)
            global.gag = 0;
        semantic3(fd, fd._scope);
        global.gag = oldgag;

        // If it is a speculatively-instantiated template, and errors occur,
        // we need to mark the template as having errors.
        if (spec && global.errors != olderrs)
            spec.errors = (global.errors - olderrs != 0);
        if (olderrs != global.errors) // if errors compiling this function
            return false;
    }

    return !fd.errors && !fd.hasSemantic3Errors();
}

// called from semantic3
/**
 * Creates and returns the hidden parameters for this function declaration.
 *
 * Hidden parameters include the `this` parameter of a class, struct or
 * nested function and the selector parameter for Objective-C methods.
 */
extern (D) void declareThis(FuncDeclaration fd, Scope* sc)
{
    const bool dualCtx = (fd.toParent2() != fd.toParentLocal());
    if (dualCtx)
        fd.hasDualContext = true;

    auto ad = fd.isThis();
    if (!dualCtx && !ad && !fd.isNested())
    {
        fd.vthis = null;
        fd.objc.selectorParameter = null;
        return;
    }

    Type addModStc(Type t)
    {
        return t.addMod(fd.type.mod).addStorageClass(fd.storage_class);
    }

    if (dualCtx || fd.isNested())
    {
        /* The 'this' for a nested function is the link to the
         * enclosing function's stack frame.
         * Note that nested functions and member functions are disjoint.
         */
        Type tthis = addModStc(dualCtx ?
                               Type.tvoidptr.sarrayOf(2).pointerTo() :
                               Type.tvoid.pointerTo());
        fd.vthis = new VarDeclaration(fd.loc, tthis, dualCtx ? Id.this2 : Id.capture, null);
        fd.vthis.storage_class |= STC.parameter | STC.nodtor;
    }
    else if (ad)
    {
        Type thandle = addModStc(ad.handleType());
        fd.vthis = new ThisDeclaration(fd.loc, thandle);
        fd.vthis.storage_class |= STC.parameter;
        if (thandle.ty == Tstruct)
        {
            fd.vthis.storage_class |= STC.ref_;
        }
    }

    if (auto tf = fd.type.isTypeFunction())
    {
        if (tf.isReturn)
            fd.vthis.storage_class |= STC.return_;
        if (tf.isScopeQual)
            fd.vthis.storage_class |= STC.scope_;
        if (tf.isReturnScope)
            fd.vthis.storage_class |= STC.returnScope;
    }

    fd.vthis.dsymbolSemantic(sc);
    if (!sc.insert(fd.vthis))
        assert(0);
    fd.vthis.parent = fd;
    if (ad)
        fd.objc.selectorParameter = .objc.createSelectorParameter(fd, sc);
}

/****************************************************
 * Check that this function type is properly resolved.
 * If not, report "forward reference error" and return true.
 */
extern (D) bool checkForwardRef(FuncDeclaration fd, Loc loc)
{
    if (!functionSemantic(fd))
        return true;

    /* No deco means the functionSemantic() call could not resolve
     * forward referenes in the type of this function.
     */
    if (!fd.type.deco)
    {
        bool inSemantic3 = (fd.inferRetType && fd.semanticRun >= PASS.semantic3);
        .error(loc, "forward reference to %s`%s`",
            (inSemantic3 ? "inferred return type of function " : "").ptr,
            fd.toChars());
        return true;
    }
    return false;
}

/*************************************************
 * Find index of function in vtbl[0..length] that
 * this function overrides.
 * Prefer an exact match to a covariant one.
 * Params:
 *      fd       = function
 *      vtbl     = vtable to use
 * Returns:
 *      -1      didn't find one
 *      -2      can't determine because of forward references
 */
int findVtblIndex(FuncDeclaration fd, Dsymbol[] vtbl)
{
    //printf("findVtblIndex() %s\n", toChars());
    import dmd.typesem : covariant;

    FuncDeclaration mismatch = null;
    STC mismatchstc = STC.none;
    int mismatchvi = -1;
    int exactvi = -1;
    int bestvi = -1;
    for (int vi = 0; vi < cast(int)vtbl.length; vi++)
    {
        FuncDeclaration fdv = vtbl[vi].isFuncDeclaration();
        if (!fdv || fdv.ident != fd.ident)
            continue;

        if (fd.type.equals(fdv.type)) // if exact match
        {
            if (fdv.parent.isClassDeclaration())
            {
                if (fdv.isFuture())
                {
                    bestvi = vi;
                    continue;           // keep looking
                }
                return vi; // no need to look further
            }

            if (exactvi >= 0)
            {
                .error(fd.loc, "%s `%s` cannot determine overridden function", fd.kind, fd.toPrettyChars);
                return exactvi;
            }
            exactvi = vi;
            bestvi = vi;
            continue;
        }

        STC stc = STC.none;
        const cov = fd.type.covariant(fdv.type, &stc);
        //printf("\tbaseclass cov = %d\n", cov);
        final switch (cov)
        {
        case Covariant.distinct:
            // types are distinct
            break;

        case Covariant.yes:
            bestvi = vi; // covariant, but not identical
            break;
            // keep looking for an exact match

        case Covariant.no:
            mismatchvi = vi;
            mismatchstc = stc;
            mismatch = fdv; // overrides, but is not covariant
            break;
            // keep looking for an exact match

        case Covariant.fwdref:
            return -2; // forward references
        }
    }
    if (fd._linkage == LINK.cpp && bestvi != -1)
    {
        STC stc = STC.none;
        FuncDeclaration fdv = vtbl[bestvi].isFuncDeclaration();
        assert(fdv && fdv.ident == fd.ident);
        if (fd.type.covariant(fdv.type, &stc, /*cppCovariant=*/true) == Covariant.no)
        {
            /* https://issues.dlang.org/show_bug.cgi?id=22351
             * Under D rules, `type` and `fdv.type` are covariant, but under C++ rules, they are not.
             * For now, continue to allow D covariant rules to apply when `override` has been used,
             * but issue a deprecation warning that this behaviour will change in the future.
             * Otherwise, follow the C++ covariant rules, which will create a new vtable entry.
             */
            if (fd.isOverride())
            {
                /* @@@DEPRECATED_2.110@@@
                 * After deprecation period has ended, be sure to remove this entire `LINK.cpp` branch,
                 * but also the `cppCovariant` parameter from Type.covariant, and update the function
                 * so that both `LINK.cpp` covariant conditions within are always checked.
                 */
                .deprecation(fd.loc, "overriding `extern(C++)` function `%s%s` with `const` qualified function `%s%s%s` is deprecated",
                             fdv.toPrettyChars(), fdv.type.toTypeFunction().parameterList.parametersTypeToChars(),
                              fd.toPrettyChars(),  fd.type.toTypeFunction().parameterList.parametersTypeToChars(), fd.type.modToChars());

                const char* where = fd.type.isNaked() ? "parameters" : "type";
                deprecationSupplemental(fd.loc, "Either remove `override`, or adjust the `const` qualifiers of the "
                                        ~ "overriding function %s", where);
            }
            else
            {
                // Treat as if Covariant.no
                mismatchvi = bestvi;
                mismatchstc = stc;
                mismatch = fdv;
                bestvi = -1;
            }
        }
    }
    if (bestvi == -1 && mismatch)
    {
        //type.print();
        //mismatch.type.print();
        //printf("%s %s\n", type.deco, mismatch.type.deco);
        //printf("stc = %llx\n", mismatchstc);
        if (mismatchstc)
        {
            // Fix it by modifying the type to add the storage classes
            fd.type = fd.type.addStorageClass(mismatchstc);
            bestvi = mismatchvi;
        }
    }
    return bestvi;
}

/*********************************
 * If function is a function in a base class,
 * return that base class.
 * Params:
 *  fd = function
 * Returns:
 *  base class if overriding, null if not
 */
BaseClass* overrideInterface(FuncDeclaration fd)
{
    for (ClassDeclaration cd = fd.toParent2().isClassDeclaration(); cd; cd = cd.baseClass)
    {
        foreach (b; cd.interfaces)
        {
            auto v = findVtblIndex(fd, b.sym.vtbl[]);
            if (v >= 0)
                return b;
        }
    }
    return null;
}

/// Flag used by $(LREF resolveFuncCall).
enum FuncResolveFlag : ubyte
{
    standard = 0,       /// issue error messages, solve the call.
    quiet = 1,          /// do not issue error message on no match, just return `null`.
    overloadOnly = 2,   /// only resolve overloads, i.e. do not issue error on ambiguous
                        /// matches and need explicit this.
    ufcs = 4,           /// trying to resolve UFCS call
}

/*******************************************
 * Given a symbol that could be either a FuncDeclaration or
 * a function template, resolve it to a function symbol.
 * Params:
 *      loc =           instantiation location
 *      sc =            instantiation scope
 *      s =             instantiation symbol
 *      tiargs =        initial list of template arguments
 *      tthis =         if !NULL, the `this` argument type
 *      argumentList =  arguments to function
 *      flags =         see $(LREF FuncResolveFlag).
 * Returns:
 *      if match is found, then function symbol, else null
 */
FuncDeclaration resolveFuncCall(Loc loc, Scope* sc, Dsymbol s,
    Objects* tiargs, Type tthis, ArgumentList argumentList, FuncResolveFlag flags)
{
    //printf("resolveFuncCall() %s\n", s.toChars());
    auto fargs = argumentList.arguments;
    if (!s)
        return null; // no match

    version (none)
    {
        printf("resolveFuncCall() %s)\n", s.toChars());
        if (tthis)
            printf("\tthis: %s\n", tthis.toChars());
        if (fargs)
        {
            for (size_t i = 0; i < fargs.length; i++)
            {
                Expression arg = (*fargs)[i];
                assert(arg.type);
                printf("\t%s: %s\n", arg.toChars(), arg.type.toChars());
            }
        }
    }

    if (tiargs && arrayObjectIsError(*tiargs))
        return null;
    if (fargs !is null)
        foreach (arg; *fargs)
            if (isError(arg))
                return null;

    MatchAccumulator m;
    functionResolve(m, s, loc, sc, tiargs, tthis, argumentList);
    auto orig_s = s;

    if (m.last > MATCH.nomatch && m.lastf)
    {
        if (m.count == 1) // exactly one match
        {
            if (!(flags & FuncResolveFlag.quiet))
                functionSemantic(m.lastf);
            return m.lastf;
        }
        if ((flags & FuncResolveFlag.overloadOnly) && !tthis && m.lastf.needThis())
        {
            return m.lastf;
        }
    }

    /* Failed to find a best match.
     * Do nothing or print error.
     */
    if (m.last == MATCH.nomatch)
    {
        // error was caused on matched function, not on the matching itself,
        // so return the function to produce a better diagnostic
        if (m.count == 1)
            return m.lastf;
    }

    // We are done at this point, as the rest of this function generate
    // a diagnostic on invalid match
    if (flags & FuncResolveFlag.quiet)
        return null;

    auto fd = s.isFuncDeclaration();
    auto od = s.isOverDeclaration();
    auto td = s.isTemplateDeclaration();
    if (td && td.funcroot)
        s = fd = td.funcroot;

    OutBuffer tiargsBuf;
    arrayObjectsToBuffer(tiargsBuf, tiargs);

    OutBuffer fargsBuf;
    fargsBuf.writeByte('(');
    argExpTypesToCBuffer(fargsBuf, fargs);
    fargsBuf.writeByte(')');
    if (tthis)
        tthis.modToBuffer(fargsBuf);

    // The call is ambiguous
    if (m.lastf && m.nextf)
    {
        TypeFunction tf1 = m.lastf.type.toTypeFunction();
        TypeFunction tf2 = m.nextf.type.toTypeFunction();
        const(char)* lastprms = parametersTypeToChars(tf1.parameterList);
        const(char)* nextprms = parametersTypeToChars(tf2.parameterList);

        string match = "";
        final switch (m.last)
        {
            case MATCH.convert:
                match = "after implicit conversions";
                break;
            case MATCH.constant:
                match = "after qualifier conversion";
                break;
            case MATCH.exact:
                match = "exactly";
                break;
            case MATCH.nomatch:
                assert(0);
        }

        .error(loc, "`%s.%s` called with argument types `%s` matches multiple overloads %.*s:\n%s:     `%s%s%s`\nand:\n%s:     `%s%s%s`",
            s.parent.toPrettyChars(), s.ident.toChars(),
            fargsBuf.peekChars(),
            match.fTuple.expand,
            m.lastf.loc.toChars(), m.lastf.toPrettyChars(), lastprms, tf1.modToChars(),
            m.nextf.loc.toChars(), m.nextf.toPrettyChars(), nextprms, tf2.modToChars());
        return null;
    }

    // no match, generate an error messages
    if (flags & FuncResolveFlag.ufcs)
    {
        auto arg = (*fargs)[0];
        .error(loc, "no property `%s` for `%s` of type `%s`", s.ident.toChars(), arg.toChars(), arg.type.toChars());
        .errorSupplemental(loc, "the following error occured while looking for a UFCS match");
    }

    if (!fd)
    {
        // all of overloads are templates
        if (td)
        {
            if (!od && !td.overnext)
            {
                .error(loc, "%s `%s` is not callable using argument types `!(%s)%s`",
                   td.kind(), td.ident.toChars(), tiargsBuf.peekChars(), fargsBuf.peekChars());
            }
            else
            {
                .error(loc, "none of the overloads of %s `%s.%s` are callable using argument types `!(%s)%s`",
                   td.kind(), td.parent.toPrettyChars(), td.ident.toChars(),
                   tiargsBuf.peekChars(), fargsBuf.peekChars());
            }


            if (!global.gag || global.params.v.showGaggedErrors)
                printCandidates(loc, td, sc.isDeprecated());
            return null;
        }
        /* This case used to happen when several ctors are mixed in an agregate.
           A (bad) error message is already generated in overloadApply().
           see https://issues.dlang.org/show_bug.cgi?id=19729
           and https://issues.dlang.org/show_bug.cgi?id=17259
        */
        if (!od)
            return null;
    }

    if (od)
    {
        .error(loc, "none of the overloads of `%s` are callable using argument types `!(%s)%s`",
               od.ident.toChars(), tiargsBuf.peekChars(), fargsBuf.peekChars());
        if (!global.gag || global.params.v.showGaggedErrors)
            printCandidates(loc, od, sc.isDeprecated());
        return null;
    }

    import dmd.expressionsem : checkDisabled;
    // remove when deprecation period of class allocators and deallocators is over
    if (fd.isNewDeclaration() && fd.checkDisabled(loc, sc))
        return null;

    bool hasOverloads = fd.overnext !is null;
    auto tf = fd.type.isTypeFunction();
    // if type is an error, the original type should be there for better diagnostics
    if (!tf)
        tf = fd.originalType.toTypeFunction();

    // modifier mismatch
    if (tthis && (fd.isCtorDeclaration() ?
        !MODimplicitConv(tf.mod, tthis.mod) :
        !MODimplicitConv(tthis.mod, tf.mod)))
    {
        OutBuffer thisBuf, funcBuf;
        MODMatchToBuffer(&thisBuf, tthis.mod, tf.mod);
        auto mismatches = MODMatchToBuffer(&funcBuf, tf.mod, tthis.mod);
        if (hasOverloads)
        {
            OutBuffer buf;
            buf.argExpTypesToCBuffer(fargs);
            if (fd.isCtorDeclaration())
                .error(loc, "none of the overloads of `%s` can construct a %sobject with argument types `(%s)`",
                    fd.toChars(), thisBuf.peekChars(), buf.peekChars());
            else
                .error(loc, "none of the overloads of `%s` are callable using a %sobject with argument types `(%s)`",
                    fd.toChars(), thisBuf.peekChars(), buf.peekChars());

            if (!global.gag || global.params.v.showGaggedErrors)
                printCandidates(loc, fd, sc.isDeprecated());
            return null;
        }

        bool calledHelper;
        void errorHelper(const(char)* failMessage) scope
        {
            .error(loc, "%s `%s%s%s` is not callable using argument types `%s`",
                   fd.kind(), fd.toPrettyChars(), parametersTypeToChars(tf.parameterList),
                   tf.modToChars(), fargsBuf.peekChars());
            errorSupplemental(loc, failMessage);
            calledHelper = true;
        }

        functionResolve(m, orig_s, loc, sc, tiargs, tthis, argumentList, &errorHelper);
        if (calledHelper)
            return null;

        if (fd.isCtorDeclaration())
            .error(loc, "%s%s `%s` cannot construct a %sobject",
                   funcBuf.peekChars(), fd.kind(), fd.toPrettyChars(), thisBuf.peekChars());
        else
            .error(loc, "%smethod `%s` is not callable using a %sobject",
                   funcBuf.peekChars(), fd.toPrettyChars(), thisBuf.peekChars());

        if (mismatches.isNotShared)
            .errorSupplemental(fd.loc, "Consider adding `shared` here");
        else if (mismatches.isMutable)
            .errorSupplemental(fd.loc, "Consider adding `const` or `inout` here");
        return null;
    }

    //printf("tf = %s, args = %s\n", tf.deco, (*fargs)[0].type.deco);
    if (hasOverloads)
    {
        .error(loc, "none of the overloads of `%s` are callable using argument types `%s`",
               fd.toChars(), fargsBuf.peekChars());
        if (!global.gag || global.params.v.showGaggedErrors)
            printCandidates(loc, fd, sc.isDeprecated());
        return null;
    }

    .error(loc, "%s `%s%s%s` is not callable using argument types `%s`",
           fd.kind(), fd.toPrettyChars(), parametersTypeToChars(tf.parameterList),
           tf.modToChars(), fargsBuf.peekChars());

    if (global.gag && !global.params.v.showGaggedErrors)
        return null;

    // re-resolve to check for supplemental message
    if (tthis)
    {
        if (auto classType = tthis.isTypeClass())
        {
            if (auto baseClass = classType.sym.baseClass)
            {
                if (auto baseFunction = baseClass.search(baseClass.loc, fd.ident))
                {
                    MatchAccumulator mErr;
                    functionResolve(mErr, baseFunction, loc, sc, tiargs, baseClass.type, argumentList);
                    if (mErr.last > MATCH.nomatch && mErr.lastf)
                    {
                        errorSupplemental(loc, "%s `%s` hides base class function `%s`",
                                fd.kind, fd.toPrettyChars(), mErr.lastf.toPrettyChars());
                        errorSupplemental(loc, "add `alias %s = %s` to `%s`'s body to merge the overload sets",
                                fd.toChars(), mErr.lastf.toPrettyChars(), tthis.toChars());
                        return null;
                    }
                }
            }
        }
    }

    void errorHelper2(const(char)* failMessage) scope
    {
        errorSupplemental(loc, failMessage);
    }

    functionResolve(m, orig_s, loc, sc, tiargs, tthis, argumentList, &errorHelper2);

    return null;
}

/*******************************************
 * Prints template and function overload candidates as supplemental errors.
 * Params:
 *      loc =            instantiation location
 *      declaration =    the declaration to print overload candidates for
 *      showDeprecated = If `false`, `deprecated` function won't be shown
 */
private void printCandidates(Decl)(Loc loc, Decl declaration, bool showDeprecated)
{
    // max num of overloads to print (-v or -verror-supplements overrides this).
    const uint DisplayLimit = global.params.v.errorSupplementCount();
    const(char)* constraintsTip;

    int printed = 0; // number of candidates printed
    int count = 0; // total candidates
    bool child; // true if inside an eponymous template
    const(char)* errorPrefix() @safe
    {
        if (child)
            return "  - Containing: ";

        // align with blank spaces after first message
        enum plural = "Candidates are: ";
        enum spaces = "                ";
        if (printed)
            return spaces;

        return (count == 1) ? "Candidate is: " : plural;
    }
    bool matchSymbol(Dsymbol s, bool print)
    {
        if (auto fd = s.isFuncDeclaration())
        {
            // Don't print overloads which have errors.
            // Not that if the whole overload set has errors, we'll never reach
            // this point so there's no risk of printing no candidate
            if (fd.errors || fd.type.ty == Terror)
                return false;
            // Don't print disabled functions, or `deprecated` outside of deprecated scope
            if (fd.storage_class & STC.disable || (fd.isDeprecated() && !showDeprecated))
                return false;
            if (!print)
                return true;
            auto tf = cast(TypeFunction) fd.type;
            OutBuffer buf;
            buf.writestring(child ? fd.toChars() : fd.toPrettyChars());
            buf.writestring(parametersTypeToChars(tf.parameterList));
            if (tf.mod)
            {
                buf.writeByte(' ');
                buf.MODtoBuffer(tf.mod);
            }
            .errorSupplemental(fd.loc, "%s`%s`", errorPrefix(), buf.peekChars());
        }
        else if (auto td = s.isTemplateDeclaration())
        {
            import dmd.staticcond;

            if (!print)
                return true;

            // if td.onemember is a function, toCharsMaybeConstraints can print it
            // without us recursing, otherwise we have to handle it.
            // td.onemember may not have overloads set
            // (see fail_compilation/onemember_overloads.d)
            // assume if more than one member it is overloaded internally
            bool recurse = td.onemember && (!td.onemember.isFuncDeclaration ||
                td.members.length > 1);
            OutBuffer buf;
            HdrGenState hgs;
            hgs.skipConstraints = true; // failing constraint should get printed below
            hgs.showOneMember = !recurse;
            toCharsMaybeConstraints(td, buf, hgs);
            const tmsg = buf.peekChars();
            const cmsg = child ? null : td.getConstraintEvalError(constraintsTip);

            if (cmsg)
                .errorSupplemental(td.loc, "%s`%s`\n%s", errorPrefix(), tmsg, cmsg);
            else
                .errorSupplemental(td.loc, "%s`%s`", errorPrefix(), tmsg);

            if (recurse)
            {
                child = true;
                foreach (d; *td.members)
                {
                    if (d.ident != td.ident)
                        continue;

                    if (auto fd2 = d.isFuncDeclaration())
                        matchSymbol(fd2, print);
                    else if (auto td2 = d.isTemplateDeclaration())
                        matchSymbol(td2, print);
                }
                child = false;
            }
        }
        return true;
    }
    // determine if there's > 1 candidate
    overloadApply(declaration, (s) {
        if (matchSymbol(s, false))
            count++;
        return count > 1;
    });
    int skipped = 0;
    overloadApply(declaration, (s) {
        if (global.params.v.verbose || printed < DisplayLimit)
        {
            if (matchSymbol(s, true))
                printed++;
        }
        else
        {
            // Too many overloads to sensibly display.
            // Just show count of remaining overloads.
            if (matchSymbol(s, false))
                skipped++;
        }
        return 0;
    });
    if (skipped > 0)
        .errorSupplemental(loc, "... (%d more, -v to show) ...", skipped);

    // Nothing was displayed, all overloads are either disabled or deprecated
    if (!printed)
        .errorSupplemental(loc, "All possible candidates are marked as `deprecated` or `@disable`");
    // should be only in verbose mode
    if (constraintsTip)
        .tip(constraintsTip);
}

/********************************************************
 * Generate Expression to call the invariant.
 * Input:
 *      ad      aggregate with the invariant
 *      vthis   variable with 'this'
 * Returns:
 *      void expression that calls the invariant
 */
Expression addInvariant(AggregateDeclaration ad, VarDeclaration vthis)
{
    Expression e = null;
    // Call invariant directly only if it exists
    FuncDeclaration inv = ad.inv;
    ClassDeclaration cd = ad.isClassDeclaration();

    while (!inv && cd)
    {
        cd = cd.baseClass;
        if (!cd)
            break;
        inv = cd.inv;
    }
    if (!inv)
        return e;

    version (all)
    {
        // Workaround for https://issues.dlang.org/show_bug.cgi?id=13394
        // For the correct mangling,
        // run attribute inference on inv if needed.
        functionSemantic(inv);
    }

    //e = new DsymbolExp(Loc.initial, inv);
    //e = new CallExp(Loc.initial, e);
    //e = e.semantic(sc2);

    /* https://issues.dlang.org/show_bug.cgi?id=13113
     * Currently virtual invariant calls completely
     * bypass attribute enforcement.
     * Change the behavior of pre-invariant call by following it.
     */
    e = new ThisExp(Loc.initial);
    e.type = ad.type.addMod(vthis.type.mod);
    e = new DotVarExp(Loc.initial, e, inv, false);
    e.type = inv.type;
    e = new CallExp(Loc.initial, e);
    e.type = Type.tvoid;
    return e;
}

/********************************************
 * Find function in overload list that exactly matches t.
 */
FuncDeclaration overloadExactMatch(FuncDeclaration thisfd, Type t)
{
    FuncDeclaration fd;
    overloadApply(thisfd, (Dsymbol s)
    {
        auto f = s.isFuncDeclaration();
        if (!f)
            return 0;
        if (f.storage_class & STC.disable)
            return 0;
        if (t.equals(f.type))
        {
            fd = f;
            return 1;
        }
        /* Allow covariant matches, as long as the return type
         * is just a const conversion.
         * This allows things like pure functions to match with an impure function type.
         */
        if (t.ty == Tfunction)
        {
            auto tf = cast(TypeFunction)f.type;
            if (tf.covariant(t) == Covariant.yes &&
                tf.nextOf().implicitConvTo(t.nextOf()) >= MATCH.constant)
            {
                fd = f;
                return 1;
            }
        }
        return 0;
    });
    return fd;
}

/****************************************************
 * Determine if fd1 overrides fd2.
 * Return !=0 if it does.
 */
int overrides(FuncDeclaration fd1, FuncDeclaration fd2)
{
    if (fd1.ident != fd2.ident)
        return 0;

    const cov = fd1.type.covariant(fd2.type);
    if (cov == Covariant.distinct)
        return 0;

    ClassDeclaration cd1 = fd1.toParent().isClassDeclaration();
    ClassDeclaration cd2 = fd2.toParent().isClassDeclaration();

    if (cd1 && cd2 && cd2.isBaseOf(cd1, null))
        return 1;
    return 0;
}

/*************************************
 * Determine partial specialization order of functions `f` vs `g`.
 * This is very similar to TemplateDeclaration::leastAsSpecialized().
 * Params:
 *  f = first function
 *  g = second function
 *  names = names of parameters
 * Returns:
 *      match   'this' is at least as specialized as g
 *      0       g is more specialized than 'this'
 */
MATCH leastAsSpecialized(FuncDeclaration f, FuncDeclaration g, Identifiers* names)
{
    enum LOG_LEASTAS = 0;
    static if (LOG_LEASTAS)
    {
        import core.stdc.stdio : printf;
        printf("leastAsSpecialized(%s, %s, %s)\n", f.toChars(), g.toChars(), names ? names.toChars() : "null");
        printf("%s, %s\n", f.type.toChars(), g.type.toChars());
    }

    /* This works by calling g() with f()'s parameters, and
     * if that is possible, then f() is at least as specialized
     * as g() is.
     */

    TypeFunction tf = f.type.toTypeFunction();
    TypeFunction tg = g.type.toTypeFunction();

    /* If both functions have a 'this' pointer, and the mods are not
     * the same and g's is not const, then this is less specialized.
     */
    if (f.needThis() && g.needThis() && tf.mod != tg.mod)
    {
        if (f.isCtorDeclaration())
        {
            if (!MODimplicitConv(tg.mod, tf.mod))
                return MATCH.nomatch;
        }
        else
        {
            if (!MODimplicitConv(tf.mod, tg.mod))
                return MATCH.nomatch;
        }
    }

    /* Create a dummy array of arguments out of the parameters to f()
     */
    Expressions args;
    foreach (u, p; tf.parameterList)
    {
        Expression e;
        if (p.isReference())
        {
            e = new IdentifierExp(Loc.initial, p.ident);
            e.type = p.type;
        }
        else
            e = p.type.defaultInitLiteral(Loc.initial);
        args.push(e);
    }

    MATCH m = callMatch(g, tg, null, ArgumentList(&args, names), 1);
    if (m > MATCH.nomatch)
    {
        /* A variadic parameter list is less specialized than a
         * non-variadic one.
         */
        if (tf.parameterList.varargs && !tg.parameterList.varargs)
            goto L1; // less specialized

        static if (LOG_LEASTAS)
        {
            printf("  matches %d, so is least as specialized\n", m);
        }
        return m;
    }
L1:
    static if (LOG_LEASTAS)
    {
        printf("  doesn't match, so is not as specialized\n");
    }
    return MATCH.nomatch;
}

/********************************************
 * Find function in overload list that matches to the 'this' modifier.
 * There's four result types.
 *
 * 1. If the 'tthis' matches only one candidate, it's an "exact match".
 *    Returns the function and 'hasOverloads' is set to false.
 *      eg. If 'tthis" is mutable and there's only one mutable method.
 * 2. If there's two or more match candidates, but a candidate function will be
 *    a "better match".
 *    Returns the better match function but 'hasOverloads' is set to true.
 *      eg. If 'tthis' is mutable, and there's both mutable and const methods,
 *          the mutable method will be a better match.
 * 3. If there's two or more match candidates, but there's no better match,
 *    Returns null and 'hasOverloads' is set to true to represent "ambiguous match".
 *      eg. If 'tthis' is mutable, and there's two or more mutable methods.
 * 4. If there's no candidates, it's "no match" and returns null with error report.
 *      e.g. If 'tthis' is const but there's no const methods.
 */
FuncDeclaration overloadModMatch(FuncDeclaration thisfd, Loc loc, Type tthis, ref bool hasOverloads)
{
    //printf("FuncDeclaration::overloadModMatch('%s')\n", toChars());
    MatchAccumulator m;
    overloadApply(thisfd, (Dsymbol s)
    {
        auto f = s.isFuncDeclaration();
        if (!f || f == m.lastf) // skip duplicates
            return 0;
        auto tf = f.type.toTypeFunction();
        //printf("tf = %s\n", tf.toChars());
        MATCH match;
        int lastIsBetter()
        {
            //printf("\tlastbetter\n");
            m.count++; // count up
            return 0;
        }
        int currIsBetter()
        {
            //printf("\tisbetter\n");
            if (m.last <= MATCH.convert)
            {
                // clear last secondary matching
                m.nextf = null;
                m.count = 0;
            }
            m.last = match;
            m.lastf = f;
            m.count++; // count up
            return 0;
        }
        if (tthis) // non-static functions are preferred than static ones
        {
            if (f.needThis())
                match = f.isCtorDeclaration() ? MATCH.exact : MODmethodConv(tthis.mod, tf.mod);
            else
                match = MATCH.constant; // keep static function in overload candidates
        }
        else // static functions are preferred than non-static ones
        {
            if (f.needThis())
                match = MATCH.convert;
            else
                match = MATCH.exact;
        }
        if (match == MATCH.nomatch)
            return 0;
        if (match > m.last) return currIsBetter();
        if (match < m.last) return lastIsBetter();
        // See if one of the matches overrides the other.
        if (m.lastf.overrides(f)) return lastIsBetter();
        if (f.overrides(m.lastf)) return currIsBetter();
        //printf("\tambiguous\n");
        m.nextf = f;
        m.count++;
        return 0;
    });
    if (m.count == 1)       // exact match
    {
        hasOverloads = false;
    }
    else if (m.count > 1)   // better or ambiguous match
    {
        hasOverloads = true;
    }
    else                    // no match
    {
        hasOverloads = true;
        auto tf = thisfd.type.toTypeFunction();
        assert(tthis);
        assert(!MODimplicitConv(tthis.mod, tf.mod)); // modifier mismatch
        {
            OutBuffer thisBuf, funcBuf;
            MODMatchToBuffer(&thisBuf, tthis.mod, tf.mod);
            MODMatchToBuffer(&funcBuf, tf.mod, tthis.mod);
            .error(loc, "%smethod %s is not callable using a %sobject", thisfd.kind, thisfd.toPrettyChars,
                funcBuf.peekChars(), thisfd.toPrettyChars(), thisBuf.peekChars());
        }
    }
    return m.lastf;
}

/***********************************
 * Determine lexical level difference from `fd` to nested function `target`.
 * Issue error if `fd` cannot call `target`.
 *
 * Params:
 *      fd = function
 *      loc = location for error messages
 *      sc = context
 *      target = target of call
 *      decl = The `Declaration` that triggered this check.
 *             Used to provide a better error message only.
 * Returns:
 *      0       same level
 *      >0      decrease nesting by number
 *      -1      increase nesting by 1 (`target` is nested within 'fd')
 *      LevelError  error
 */
int getLevelAndCheck(FuncDeclaration fd, Loc loc, Scope* sc, FuncDeclaration target,
                     Declaration decl)
{
    int level = fd.getLevel(target, sc.intypeof);
    if (level != fd.LevelError)
        return level;
    // Don't give error if in template constraint
    if (!sc.inTemplateConstraint)
    {
        const(char)* xstatic = fd.isStatic() ? "`static` " : "";
        // better diagnostics for static functions
        .error(loc, "%s%s `%s` cannot access %s `%s` in frame of function `%s`",
               xstatic, fd.kind(), fd.toPrettyChars(), decl.kind(), decl.toChars(),
               target.toPrettyChars());
            .errorSupplemental(decl.loc, "`%s` declared here", decl.toChars());
        return fd.LevelError;
    }
    return 1;
}

/**********************************
 * Decide if attributes for this function can be inferred from examining
 * the function body.
 * Params:
 *      fd = function to infer attributes for
 *      sc = context
 * Returns:
 *  true if can
 */
bool canInferAttributes(FuncDeclaration fd, Scope* sc)
{
    if (!fd.fbody)
        return false;
    if (fd.isVirtualMethod() &&
        /*
         * https://issues.dlang.org/show_bug.cgi?id=21719
         *
         * If we have an auto virtual function we can infer
         * the attributes.
         */
        !(fd.inferRetType && !fd.isCtorDeclaration()))
        return false;               // since they may be overridden
    if (sc.func &&
        /********** this is for backwards compatibility for the moment ********/
        (!fd.isMember() || sc.func.isSafeBypassingInference() && !fd.isInstantiated()))
        return true;
    if (fd.isFuncLiteralDeclaration() ||               // externs are not possible with literals
        (fd.storage_class & STC.inference) ||          // do attribute inference
        fd.isGenerated ||                              // compiler generated function
        (fd.inferRetType && !fd.isCtorDeclaration()))
        return true;
    if (fd.isInstantiated())
    {
        auto ti = fd.parent.isTemplateInstance();
        if (ti is null || ti.isTemplateMixin() || ti.tempdecl.ident == fd.ident)
            return true;
    }
    return false;
}

/*********************************************
 * In the current function 'sc.func', we are calling 'fd'.
 * 1. Check to see if the current function can call 'fd' , issue error if not.
 * 2. If the current function is not the parent of 'fd' , then add
 *    the current function to the list of siblings of 'fd' .
 * 3. If the current function is a literal, and it's accessing an uplevel scope,
 *    then mark it as a delegate.
 * Returns true if error occurs.
 */
bool checkNestedFuncReference(FuncDeclaration fd, Scope* sc, Loc loc)
{
    //printf("FuncDeclaration::checkNestedFuncReference() %s\n", toPrettyChars());
    if (auto fld = fd.isFuncLiteralDeclaration())
    {
        if (fld.tok == TOK.reserved)
        {
            fld.tok = TOK.function_;
            fld.vthis = null;
        }
    }
    if (!fd.parent || fd.parent == sc.parent)
        return false;
    if (fd.ident == Id.require || fd.ident == Id.ensure)
        return false;
    if (!fd.isThis() && !fd.isNested())
        return false;
    // The current function
    FuncDeclaration fdthis = sc.parent.isFuncDeclaration();
    if (!fdthis)
        return false; // out of function scope
    Dsymbol p = fd.toParentLocal();
    Dsymbol p2 = fd.toParent2();
    // Function literals from fdthis to p must be delegates
    ensureStaticLinkTo(fdthis, p);
    if (p != p2)
        ensureStaticLinkTo(fdthis, p2);
    if (!fd.isNested())
        return false;

    // The function that this function is in
    bool checkEnclosing(FuncDeclaration fdv)
    {
        if (!fdv)
            return false;
        if (fdv == fdthis)
            return false;
        //printf("this = %s in [%s]\n", this.toChars(), this.loc.toChars());
        //printf("fdv  = %s in [%s]\n", fdv .toChars(), fdv .loc.toChars());
        //printf("fdthis = %s in [%s]\n", fdthis.toChars(), fdthis.loc.toChars());
        // Add this function to the list of those which called us
        if (fdthis != fd)
        {
            bool found = false;
            for (size_t i = 0; i < fd.siblingCallers.length; ++i)
            {
                if (fd.siblingCallers[i] == fdthis)
                    found = true;
            }
            if (!found)
            {
                //printf("\tadding sibling %s to %s\n", fdthis.toPrettyChars(), toPrettyChars());
                if (!sc.intypeof && !sc.traitsCompiles)
                {
                    fd.siblingCallers.push(fdthis);
                    fd.computedEscapingSiblings = false;
                }
            }
        }
        const lv = fdthis.getLevelAndCheck(loc, sc, fdv, fd);
        if (lv == fd.LevelError)
            return true; // error
        if (lv == -1)
            return false; // downlevel call
        if (lv == 0)
            return false; // same level call
        return false; // Uplevel call
    }
    if (checkEnclosing(p.isFuncDeclaration()))
        return true;
    if (checkEnclosing(p == p2 ? null : p2.isFuncDeclaration()))
        return true;
    return false;
}

/****************************************************
 * Check whether result variable can be built.
 * Returns:
 *     `true` if the function has a return type that
 *     is different from `void`.
 */
private bool canBuildResultVar(FuncDeclaration fd)
{
    auto f = cast(TypeFunction)fd.type;
    return f && f.nextOf() && f.nextOf().toBasetype().ty != Tvoid;
}

/****************************************************
 * Declare result variable lazily.
 */
void buildResultVar(FuncDeclaration fd, Scope* sc, Type tret)
{
    if (!fd.vresult)
    {
        Loc loc = fd.fensure ? fd.fensure.loc : fd.loc;
        /* If inferRetType is true, tret may not be a correct return type yet.
         * So, in here it may be a temporary type for vresult, and after
         * fbody.dsymbolSemantic() running, vresult.type might be modified.
         */
        fd.vresult = new VarDeclaration(loc, tret, Id.result, null);
        fd.vresult.storage_class |= STC.nodtor | STC.temp;
        if (!fd.isVirtual())
            fd.vresult.storage_class |= STC.const_;
        fd.vresult.storage_class |= STC.result;
        // set before the semantic() for checkNestedReference()
        fd.vresult.parent = fd;
    }
    if (sc && fd.vresult.semanticRun == PASS.initial)
    {
        TypeFunction tf = fd.type.toTypeFunction();
        if (tf.isRef)
            fd.vresult.storage_class |= STC.ref_;
        fd.vresult.type = tret;
        fd.vresult.dsymbolSemantic(sc);
        if (!sc.insert(fd.vresult))
            .error(fd.loc, "%s `%s` out result %s is already defined", fd.kind, fd.toPrettyChars, fd.vresult.toChars());
        assert(fd.vresult.parent == fd);
    }
}

/****************************************************
 * Merge into this function the 'in' contracts of all it overrides.
 * 'in's are OR'd together, i.e. only one of them needs to pass.
 */
Statement mergeFrequire(FuncDeclaration fd, Statement sf, Expressions* params)
{
    /* If a base function and its override both have an IN contract, then
     * only one of them needs to succeed. This is done by generating:
     *
     * void derived.in() {
     *  try {
     *    base.in();
     *  }
     *  catch () {
     *    ... body of derived.in() ...
     *  }
     * }
     *
     * So if base.in() doesn't throw, derived.in() need not be executed, and the contract is valid.
     * If base.in() throws, then derived.in()'s body is executed.
     */
    foreach (fdv; fd.foverrides)
    {
        /* The semantic pass on the contracts of the overridden functions must
         * be completed before code generation occurs.
         * https://issues.dlang.org/show_bug.cgi?id=3602
         */
        if (fdv.frequires && fdv.semanticRun != PASS.semantic3done)
        {
            assert(fdv._scope);
            Scope* sc = fdv._scope.push();
            sc.stc &= ~STC.override_;
            fdv.semantic3(sc);
            sc.pop();
        }
        sf = fdv.mergeFrequire(sf, params);
        if (!sf || !fdv.fdrequire)
            return null;
        //printf("fdv.frequire: %s\n", fdv.frequire.toChars());
        /* Make the call:
            *   try { __require(params); }
            *   catch (Throwable) { frequire; }
            */
        params = Expression.arraySyntaxCopy(params);
        Expression e = new CallExp(fd.loc, new VarExp(fd.loc, fdv.fdrequire, false), params);
        Statement s2 = new ExpStatement(fd.loc, e);
        auto c = new Catch(fd.loc, getThrowable(), null, sf);
        c.internalCatch = true;
        auto catches = new Catches();
        catches.push(c);
        sf = new TryCatchStatement(fd.loc, s2, catches);
    }
    return sf;
}

/****************************************************
 * Merge into this function the 'in' contracts of all it overrides.
 */
Statement mergeFrequireInclusivePreview(FuncDeclaration fd, Statement sf, Expressions* params)
{
    /* If a base function and its override both have an IN contract, then
     * the override in contract must widen the guarantee of the base contract.
     * This is checked by generating:
     *
     * void derived.in() {
     *  try {
     *    ... body of derived.in() ...
     *  }
     *  catch () {
     *    // derived in rejected this argument. so parent must also reject it, or we've tightened the contract.
     *    base.in();
     *    assert(false, "Logic error: " ~ thr.msg);
     *  }
     * }
     */
    foreach (fdv; fd.foverrides)
    {
        /* The semantic pass on the contracts of the overridden functions must
         * be completed before code generation occurs.
         * https://issues.dlang.org/show_bug.cgi?id=3602
         */
        if (fdv.frequires && fdv.semanticRun != PASS.semantic3done)
        {
            assert(fdv._scope);
            Scope* sc = fdv._scope.push();
            sc.stc &= ~STC.override_;
            fdv.semantic3(sc);
            sc.pop();
        }
        sf = fdv.mergeFrequireInclusivePreview(sf, params);
        if (!sf || !fdv.fdrequire)
            return null;

        const loc = fd.fdrequire.loc;
        //printf("fdv.frequire: %s\n", fdv.frequire.toChars());
        /* Make the call:
         *   try { frequire; }
         *   catch (Throwable thr) { __require(params); assert(false, "Logic error: " ~ thr.msg); }
         */
        Identifier id = Identifier.generateId("thr");
        params = Expression.arraySyntaxCopy(params);
        Expression e = new CallExp(loc, new VarExp(loc, fdv.fdrequire, false), params);
        Statement s2 = new ExpStatement(loc, e);
        // assert(false, ...)
        // TODO make this a runtime helper to allow:
        // - chaining the original expression
        // - nogc concatenation
        Expression msg = new StringExp(loc, "Logic error: in-contract was tighter than parent in-contract");
        Statement fail = new ExpStatement(loc, new AssertExp(loc, IntegerExp.literal!0, msg));
        Statement s3 = new CompoundStatement(loc, s2, fail);
        auto c = new Catch(loc, getThrowable(), id, s3);
        c.internalCatch = true;
        auto catches = new Catches();
        catches.push(c);
        sf = new TryCatchStatement(loc, sf, catches);
    }
    return sf;
}

/****************************************************
 * Rewrite contracts as statements.
 */
void buildEnsureRequire(FuncDeclaration thisfd)
{
    if (thisfd.frequires)
    {
        /*   in { statements1... }
         *   in { statements2... }
         *   ...
         * becomes:
         *   in { { statements1... } { statements2... } ... }
         */
        assert(thisfd.frequires.length);
        auto loc = (*thisfd.frequires)[0].loc;
        auto s = new Statements;
        foreach (r; *thisfd.frequires)
        {
            s.push(new ScopeStatement(r.loc, r, r.loc));
        }
        thisfd.frequire = new CompoundStatement(loc, s);
    }
    if (thisfd.fensures)
    {
        /*   out(id1) { statements1... }
         *   out(id2) { statements2... }
         *   ...
         * becomes:
         *   out(__result) { { ref id1 = __result; { statements1... } }
         *                   { ref id2 = __result; { statements2... } } ... }
         */
        assert(thisfd.fensures.length);
        auto loc = (*thisfd.fensures)[0].ensure.loc;
        auto s = new Statements;
        foreach (r; *thisfd.fensures)
        {
            if (r.id && thisfd.canBuildResultVar())
            {
                auto rloc = r.ensure.loc;
                auto resultId = new IdentifierExp(rloc, Id.result);
                auto init = new ExpInitializer(rloc, resultId);
                auto stc = STC.ref_ | STC.temp | STC.result;
                auto decl = new VarDeclaration(rloc, null, r.id, init, stc);
                auto sdecl = new ExpStatement(rloc, decl);
                s.push(new ScopeStatement(rloc, new CompoundStatement(rloc, sdecl, r.ensure), rloc));
            }
            else
            {
                s.push(r.ensure);
            }
        }
        thisfd.fensure = new CompoundStatement(loc, s);
    }
    if (!thisfd.isVirtual())
        return;
    /* Rewrite contracts as nested functions, then call them. Doing it as nested
     * functions means that overriding functions can call them.
     */
    auto f = cast(TypeFunction) thisfd.type;
    /* Make a copy of the parameters and make them all ref */
    static Parameters* toRefCopy(ParameterList parameterList)
    {
        auto result = new Parameters();
        foreach (n, p; parameterList)
        {
            p = p.syntaxCopy();
            if (!p.isLazy())
                p.storageClass = (p.storageClass | STC.ref_) & ~STC.out_;
            p.defaultArg = null; // won't be the same with ref
            result.push(p);
        }
        return result;
    }
    if (thisfd.frequire)
    {
        /*   in { ... }
         * becomes:
         *   void __require(ref params) { ... }
         *   __require(params);
         */
        Loc loc = thisfd.frequire.loc;
        thisfd.fdrequireParams = new Expressions();
        if (thisfd.parameters)
        {
            foreach (vd; *thisfd.parameters)
                thisfd.fdrequireParams.push(new VarExp(loc, vd));
        }
        auto fo = cast(TypeFunction)(thisfd.originalType ? thisfd.originalType : f);
        auto fparams = toRefCopy(fo.parameterList);
        auto tf = new TypeFunction(ParameterList(fparams), Type.tvoid, LINK.d);
        tf.isNothrow = f.isNothrow;
        tf.isNogc = f.isNogc;
        tf.purity = f.purity;
        tf.trust = f.trust;
        auto fd = new FuncDeclaration(loc, loc, Id.require, STC.none, tf);
        fd.fbody = thisfd.frequire;
        Statement s1 = new ExpStatement(loc, fd);
        Expression e = new CallExp(loc, new VarExp(loc, fd, false), thisfd.fdrequireParams);
        Statement s2 = new ExpStatement(loc, e);
        thisfd.frequire = new CompoundStatement(loc, s1, s2);
        thisfd.fdrequire = fd;
    }
    /* We need to set fdensureParams here and not in the block below to
     * have the parameters available when calling a base class ensure(),
     * even if this function doesn't have an out contract.
     */
    thisfd.fdensureParams = new Expressions();
    if (thisfd.canBuildResultVar())
        thisfd.fdensureParams.push(new IdentifierExp(thisfd.loc, Id.result));
    if (thisfd.parameters)
    {
        foreach (vd; *thisfd.parameters)
            thisfd.fdensureParams.push(new VarExp(thisfd.loc, vd));
    }
    if (thisfd.fensure)
    {
        /*   out (result) { ... }
         * becomes:
         *   void __ensure(ref tret result, ref params) { ... }
         *   __ensure(result, params);
         */
        Loc loc = thisfd.fensure.loc;
        auto fparams = new Parameters();
        if (thisfd.canBuildResultVar())
        {
            Parameter p = new Parameter(loc, STC.ref_ | STC.const_, f.nextOf(), Id.result, null, null);
            fparams.push(p);
        }
        auto fo = cast(TypeFunction)(thisfd.originalType ? thisfd.originalType : f);
        fparams.pushSlice((*toRefCopy(fo.parameterList))[]);
        auto tf = new TypeFunction(ParameterList(fparams), Type.tvoid, LINK.d);
        tf.isNothrow = f.isNothrow;
        tf.isNogc = f.isNogc;
        tf.purity = f.purity;
        tf.trust = f.trust;
        auto fd = new FuncDeclaration(loc, loc, Id.ensure, STC.none, tf);
        fd.fbody = thisfd.fensure;
        Statement s1 = new ExpStatement(loc, fd);
        Expression e = new CallExp(loc, new VarExp(loc, fd, false), thisfd.fdensureParams);
        Statement s2 = new ExpStatement(loc, e);
        thisfd.fensure = new CompoundStatement(loc, s1, s2);
        thisfd.fdensure = fd;
    }
}

/****************************************************
 * Determine whether an 'out' contract is declared inside
 * the given function or any of its overrides.
 * Params:
 *      fd = the function to search
 * Returns:
 *      true    found an 'out' contract
 */
bool needsFensure(FuncDeclaration fd) @safe
{
    if (fd.fensures)
        return true;

    foreach (fdv; fd.foverrides)
    {
        if (needsFensure(fdv))
            return true;
    }
    return false;
}

/****************************************************
 * Merge into this function the 'out' contracts of all it overrides.
 * 'out's are AND'd together, i.e. all of them need to pass.
 */
Statement mergeFensure(FuncDeclaration fd, Statement sf, Identifier oid, Expressions* params)
{
    /* Same comments as for mergeFrequire(), except that we take care
     * of generating a consistent reference to the 'result' local by
     * explicitly passing 'result' to the nested function as a reference
     * argument.
     * This won't work for the 'this' parameter as it would require changing
     * the semantic code for the nested function so that it looks on the parameter
     * list for the 'this' pointer, something that would need an unknown amount
     * of tweaking of various parts of the compiler that I'd rather leave alone.
     */
    foreach (fdv; fd.foverrides)
    {
        /* The semantic pass on the contracts of the overridden functions must
         * be completed before code generation occurs.
         * https://issues.dlang.org/show_bug.cgi?id=3602 and
         * https://issues.dlang.org/show_bug.cgi?id=5230
         */
        if (needsFensure(fdv) && fdv.semanticRun != PASS.semantic3done)
        {
            assert(fdv._scope);
            Scope* sc = fdv._scope.push();
            sc.stc &= ~STC.override_;
            fdv.semantic3(sc);
            sc.pop();
        }
        sf = fdv.mergeFensure(sf, oid, params);
        if (!fdv.fdensure)
            continue;

        //printf("fdv.fensure: %s\n", fdv.fensure.toChars());
        // Make the call: __ensure(result, params)
        params = Expression.arraySyntaxCopy(params);
        if (fd.canBuildResultVar())
        {
            Type t1 = fdv.type.nextOf().toBasetype();
            Type t2 = fd.type.nextOf().toBasetype();
            if (t1.isBaseOf(t2, null))
            {
                /* Making temporary reference variable is necessary
                 * in covariant return.
                 * https://issues.dlang.org/show_bug.cgi?id=5204
                 * https://issues.dlang.org/show_bug.cgi?id=10479
                 */
                Expression* eresult = &(*params)[0];
                auto ei = new ExpInitializer(Loc.initial, *eresult);
                auto v = new VarDeclaration(Loc.initial, t1, Identifier.generateId("__covres"), ei);
                v.storage_class |= STC.temp;
                auto de = new DeclarationExp(Loc.initial, v);
                auto ve = new VarExp(Loc.initial, v);
                *eresult = new CommaExp(Loc.initial, de, ve);
            }
        }
        Expression e = new CallExp(fd.loc, new VarExp(fd.loc, fdv.fdensure, false), params);
        Statement s2 = new ExpStatement(fd.loc, e);
        if (sf)
        {
            sf = new CompoundStatement(sf.loc, s2, sf);
        }
        else
            sf = s2;
    }
    return sf;
}

/*******************************
 * Modify all expression type of return statements to tret.
 *
 * On function literals, return type may be modified based on the context type
 * after its semantic3 is done, in FuncExp::implicitCastTo.
 *
 *  A function() dg = (){ return new B(); } // OK if is(B : A) == true
 *
 * If B to A conversion is convariant that requires offseet adjusting,
 * all return statements should be adjusted to return expressions typed A.
 */
void modifyReturns(FuncLiteralDeclaration fld, Scope* sc, Type tret)
{
    extern (C++) final class RetWalker : StatementRewriteWalker
    {
        alias visit = typeof(super).visit;
    public:
        Scope* sc;
        Type tret;
        FuncLiteralDeclaration fld;
        override void visit(ReturnStatement s)
        {
            Expression exp = s.exp;
            if (exp && !exp.type.equals(tret))
                s.exp = exp.implicitCastTo(sc, tret);
        }
    }
    if (fld.semanticRun < PASS.semantic3done)
        return;
    if (fld.fes)
        return;
    scope RetWalker w = new RetWalker();
    w.sc = sc;
    w.tret = tret;
    w.fld = fld;
    fld.fbody.accept(w);
    // Also update the inferred function type to match the new return type.
    // This is required so the code generator does not try to cast the
    // modified returns back to the original type.
    if (fld.inferRetType && fld.type.nextOf() != tret)
        fld.type.toTypeFunction().next = tret;
}

/**************************************
 * When a traits(compiles) is used on a function literal call
 * we need to take into account if the body of the function
 * violates any attributes, however, we must not affect the
 * attribute inference on the outer function. The attributes
 * of the function literal still need to be inferred, therefore
 * we need a way to check for the scope that the traits compiles
 * introduces.
 *
 * Params:
 *   sc = scope to be checked for
 *
 * Returns: `true` if the provided scope is the root
 * of the traits compiles list of scopes.
 */
bool isRootTraitsCompilesScope(Scope* sc) @safe
{
    return (sc.traitsCompiles) && !sc.func.skipCodegen;
}

/+
 + Checks the parameter and return types iff this is a `main` function.
 +
 + The following signatures are allowed for a `D main`:
 + - Either no or a single parameter of type `string[]`
 + - Return type is either `void`, `int` or `noreturn`
 +
 + The following signatures are standard C:
 + - `int main()`
 + - `int main(int, char**)`
 +
 + This function accepts the following non-standard extensions:
 + - `char** envp` as a third parameter
 + - `void` / `noreturn` as return type
 +
 + This function will issue errors for unexpected arguments / return types.
 +/
extern (D) void checkMain(FuncDeclaration fd)
{
    if (fd.ident != Id.main || fd.isMember() || fd.isNested())
        return; // Not a main function

    TypeFunction tf = fd.type.toTypeFunction();

    Type retType = tf.nextOf();
    if (!retType)
    {
        // auto main(), check after semantic
        assert(fd.inferRetType);
        return;
    }

    /// Checks whether `t` is equivalent to `char**`
    /// Ignores qualifiers and treats enums according to their base type
    static bool isCharPtrPtr(Type t)
    {
        auto tp = t.toBasetype().isTypePointer();
        if (!tp)
            return false;

        tp = tp.next.toBasetype().isTypePointer();
        if (!tp)
            return false;

        return tp.next.toBasetype().ty == Tchar;
    }

    // Neither of these qualifiers is allowed because they affect the ABI
    enum invalidSTC = STC.out_ | STC.ref_ | STC.lazy_;

    const nparams = tf.parameterList.length;
    bool argerr;

    const linkage = fd.resolvedLinkage();
    if (linkage == LINK.d)
    {
        if (nparams == 1)
        {
            auto fparam0 = tf.parameterList[0];
            auto t = fparam0.type.toBasetype();
            if (t.ty != Tarray ||
                t.nextOf().ty != Tarray ||
                t.nextOf().nextOf().ty != Tchar ||
                fparam0.storageClass & invalidSTC)
            {
                argerr = true;
            }
        }

        if (tf.parameterList.varargs || nparams >= 2 || argerr)
            .error(fd.loc, "%s `%s` parameter list must be empty or accept one parameter of type `string[]`", fd.kind, fd.toPrettyChars);
    }

    else if (linkage == LINK.c)
    {
        if (nparams == 2 || nparams == 3)
        {
            // Argument count must be int
            auto argCount = tf.parameterList[0];
            argerr |= !!(argCount.storageClass & invalidSTC);
            argerr |= argCount.type.toBasetype().ty != Tint32;

            // Argument pointer must be char**
            auto argPtr = tf.parameterList[1];
            argerr |= !!(argPtr.storageClass & invalidSTC);
            argerr |= !isCharPtrPtr(argPtr.type);

            // `char** environ` is a common extension, see J.5.1 of the C standard
            if (nparams == 3)
            {
                auto envPtr = tf.parameterList[2];
                argerr |= !!(envPtr.storageClass & invalidSTC);
                argerr |= !isCharPtrPtr(envPtr.type);
            }
        }
        else
            argerr = nparams != 0;

        // Disallow variadic main() - except for K&R declarations in C files.
        // E.g. int main(), int main(argc, argv) int argc, char** argc { ... }
        if (tf.parameterList.varargs && (!fd.isCsymbol() || (!tf.parameterList.hasIdentifierList && nparams)))
            argerr |= true;

        if (argerr)
        {
            .error(fd.loc, "%s `%s` parameters must match one of the following signatures", fd.kind, fd.toPrettyChars);
            fd.loc.errorSupplemental("`main()`");
            fd.loc.errorSupplemental("`main(int argc, char** argv)`");
            fd.loc.errorSupplemental("`main(int argc, char** argv, char** environ)` [POSIX extension]");
        }
    }
    else
        return; // Neither C nor D main, ignore (should probably be an error)

    // Allow enums with appropriate base types (same ABI)
    retType = retType.toBasetype();

    if (retType.ty != Tint32 && retType.ty != Tvoid && retType.ty != Tnoreturn)
        .error(fd.loc, "%s `%s` must return `int`, `void` or `noreturn`, not `%s`", fd.kind, fd.toPrettyChars, tf.nextOf().toChars());
}

/***********************************************
 * Check all return statements for a function to verify that returning
 * using NRVO is possible.
 *
 * Returns:
 *      `false` if the result cannot be returned by hidden reference.
 */
extern (D) bool checkNRVO(FuncDeclaration fd)
{
    //printf("checkNRVO*() %s\n", fd.ident.toChars());
    if (!fd.isNRVO() || fd.returns is null)
        return false;

    auto tf = fd.type.toTypeFunction();
    if (tf.isRef)
        return false;

    foreach (rs; *fd.returns)
    {
        if (auto ve = rs.exp.isVarExp())
        {
            auto v = ve.var.isVarDeclaration();
            if (!v || v.isReference())
                return false;
            if (fd.nrvo_var is null)
            {
                // Variables in the data segment (e.g. globals, TLS or not),
                // parameters and closure variables cannot be NRVOed.
                if (v.isDataseg() || v.isParameter() || v.toParent2() != fd)
                    return false;
                if (v.nestedrefs.length && fd.needsClosure())
                    return false;
                // don't know if the return storage is aligned
                version (MARS)
                {
                    if (fd.alignSectionVars && (*fd.alignSectionVars).contains(v))
                        return false;
                }
                // The variable type needs to be equivalent to the return type.
                if (!v.type.equivalent(tf.next))
                    return false;
                //printf("Setting nrvo to %s\n", v.toChars());
                fd.nrvo_var = v;
            }
            else if (fd.nrvo_var != v)
                return false;
        }
        else //if (!exp.isLvalue())    // keep NRVO-ability
            return false;
    }
    return true;
}

/**************************************
 * The function is doing something impure, so mark it as impure.
 *
 * Params:
 *     fd = function declaration to mark
 *     loc = location of impure action
 *     fmt = format string for error message
 *     args = argument to format string
 *
 * Returns: `true` if there's a purity error
 */
extern (D) bool setImpure(FuncDeclaration fd, Loc loc, const(char)* fmt, RootObject[] args...)
{
    if (fd.purityInprocess)
    {
        fd.purityInprocess = false;
        if (fmt)
            fd.pureViolation = new AttributeViolation(loc, fmt, args); // impure action
        else if (args.length > 0)
        {
            if (auto sa = args[0].isDsymbol())
            {
                if (FuncDeclaration fd2 = sa.isFuncDeclaration())
                {
                    fd.pureViolation = new AttributeViolation(loc, fd2); // call to impure function
                }
            }
        }

        if (fd.fes)
            fd.fes.func.setImpure(loc, fmt, args);
    }
    else if (fd.isPure())
        return true;
    return false;
}

PURE isPure(FuncDeclaration fd)
{
    //printf("FuncDeclaration::isPure() '%s'\n", toChars());


    TypeFunction tf = fd.type.toTypeFunction();
    if (fd.purityInprocess)
        fd.setImpure(Loc.initial, null);
    if (tf.purity == PURE.fwdref)
        tf.purityLevel();
    PURE purity = tf.purity;
    if (purity > PURE.weak && fd.isNested())
        purity = PURE.weak;
    if (purity > PURE.weak && fd.needThis())
    {
        // The attribute of the 'this' reference affects purity strength
        if (fd.type.mod & MODFlags.immutable_)
        {
        }
        else if (fd.type.mod & (MODFlags.const_ | MODFlags.wild) && purity >= PURE.const_)
            purity = PURE.const_;
        else
            purity = PURE.weak;
    }
    tf.purity = purity;
    // ^ This rely on the current situation that every FuncDeclaration has a
    //   unique TypeFunction.
    return purity;
}

extern (D) PURE isPureBypassingInference(FuncDeclaration fd)
{
    if (fd.purityInprocess)
        return PURE.fwdref;
    else
        return fd.isPure();
}

/**************************************
 * Performs type-based alias analysis between a newly created value and a pre-
 * existing memory reference:
 *
 * Assuming that a reference A to a value of type `ta` was available to the code
 * that created a reference B to a value of type `tb`, it returns whether B
 * might alias memory reachable from A based on the types involved (either
 * directly or via any number of indirections in either A or B).
 *
 * This relation is not symmetric in the two arguments. For example, a
 * a `const(int)` reference can point to a pre-existing `int`, but not the other
 * way round.
 *
 * Examples:
 *
 *      ta,           tb,               result
 *      `const(int)`, `int`,            `false`
 *      `int`,        `const(int)`,     `true`
 *      `int`,        `immutable(int)`, `false`
 *      const(immutable(int)*), immutable(int)*, false   // BUG: returns true
 *
 * Params:
 *      ta = value type being referred to
 *      tb = referred to value type that could be constructed from ta
 *
 * Returns:
 *      true if reference to `tb` is isolated from reference to `ta`
 */
bool traverseIndirections(Type ta, Type tb)
{
    //printf("traverseIndirections(%s, %s)\n", ta.toChars(), tb.toChars());

    static bool traverse(Type ta, Type tb, ref scope AssocArray!(const(char)*, bool) table, bool reversePass)
    {
        //printf("traverse(%s, %s)\n", ta.toChars(), tb.toChars());
        ta = ta.baseElemOf();
        tb = tb.baseElemOf();

        // First, check if the pointed-to types are convertible to each other such
        // that they might alias directly.
        static bool mayAliasDirect(Type source, Type target)
        {
            return
                // if source is the same as target or can be const-converted to target
                source.constConv(target) != MATCH.nomatch ||
                // if target is void and source can be const-converted to target
                (target.ty == Tvoid && MODimplicitConv(source.mod, target.mod));
        }

        if (mayAliasDirect(reversePass ? tb : ta, reversePass ? ta : tb))
        {
            //printf(" true  mayalias %s %s %d\n", ta.toChars(), tb.toChars(), reversePass);
            return false;
        }
        if (ta.nextOf() && ta.nextOf() == tb.nextOf())
        {
             //printf(" next==next %s %s %d\n", ta.toChars(), tb.toChars(), reversePass);
             return true;
        }

        if (tb.ty == Tclass || tb.ty == Tstruct)
        {
            /* Traverse the type of each field of the aggregate
             */
            bool* found = table.getLvalue(tb.deco);
            if (*found == true)
                return true; // We have already seen this symbol, break the cycle
            else
                *found = true;

            AggregateDeclaration sym = tb.toDsymbol(null).isAggregateDeclaration();
            foreach (v; sym.fields)
            {
                Type tprmi = v.type.addMod(tb.mod);
                //printf("\ttb = %s, tprmi = %s\n", tb.toChars(), tprmi.toChars());
                if (!traverse(ta, tprmi, table, reversePass))
                    return false;
            }
        }
        else if (tb.ty == Tarray || tb.ty == Taarray || tb.ty == Tpointer)
        {
            Type tind = tb.nextOf();
            if (!traverse(ta, tind, table, reversePass))
                return false;
        }
        else if (tb.hasPointers())
        {
            // BUG: consider the context pointer of delegate types
            return false;
        }

        // Still no match, so try breaking up ta if we have not done so yet.
        if (!reversePass)
        {
            scope newTable = AssocArray!(const(char)*, bool)();
            return traverse(tb, ta, newTable, true);
        }

        return true;
    }

    // To handle arbitrary levels of indirections in both parameters, we
    // recursively descend into aggregate members/levels of indirection in both
    // `ta` and `tb` while avoiding cycles. Start with the original types.
    scope table = AssocArray!(const(char)*, bool)();
    const result = traverse(ta, tb, table, false);
    //printf("  returns %d\n", result);
    return result;
}

/********************************************
 * Params:
 *     fd = function declaration to check
 *    t = type of object to test one level of indirection down
 * Returns:
 *    true if an object typed `t` has no indirections
 *    which could have come from the function's parameters, mutable
 *    globals, or uplevel functions.
 */
bool isTypeIsolatedIndirect(FuncDeclaration fd, Type t)
{
    //printf("isTypeIsolatedIndirect(t: %s)\n", t.toChars());
    assert(t);

    /* Since `t` is one level down from an indirection, it could pick
     * up a reference to a mutable global or an outer function, so
     * return false.
     */
    if (!fd.isPureBypassingInference() || fd.isNested())
        return false;

    TypeFunction tf = fd.type.toTypeFunction();

    //printf("isTypeIsolatedIndirect(%s) t = %s\n", tf.toChars(), t.toChars());

    foreach (i, fparam; tf.parameterList)
    {
        Type tp = fparam.type;
        if (!tp)
            continue;

        if (fparam.isLazy() || fparam.isReference())
        {
            if (!traverseIndirections(tp, t))
                return false;
            continue;
        }

        /* Goes down one level of indirection, then calls traverseIndirection() on
         * the result.
         * Returns:
         *  true if t is isolated from tp
         */
        static bool traverse(Type tp, Type t)
        {
            tp = tp.baseElemOf();
            switch (tp.ty)
            {
                case Tarray:
                case Tpointer:
                    return traverseIndirections(tp.nextOf(), t);

                case Taarray:
                case Tclass:
                    return traverseIndirections(tp, t);

                case Tstruct:
                    /* Drill down and check the struct's fields
                     */
                    auto sym = tp.toDsymbol(null).isStructDeclaration();
                    foreach (v; sym.fields)
                    {
                        Type tprmi = v.type.addMod(tp.mod);
                        //printf("\ttp = %s, tprmi = %s\n", tp.toChars(), tprmi.toChars());
                        if (!traverse(tprmi, t))
                            return false;
                    }
                    return true;

                default:
                    return true;
            }
        }

        if (!traverse(tp, t))
            return false;
    }
    // The 'this' reference is a parameter, too
    if (AggregateDeclaration ad = fd.isCtorDeclaration() ? null : fd.isThis())
    {
        Type tthis = ad.getType().addMod(tf.mod);
        //printf("\ttthis = %s\n", tthis.toChars());
        if (!traverseIndirections(tthis, t))
            return false;
    }

    return true;
}

/********************************************
 * See if pointers from function parameters, mutable globals, or uplevel functions
 * could leak into return value.
 * Returns:
 *   true if the function return value is isolated from
 *   any inputs to the function
 */
extern (D) bool isReturnIsolated(FuncDeclaration fd)
{
    //printf("isReturnIsolated(this: %s)\n", this.toChars);
    TypeFunction tf = fd.type.toTypeFunction();
    assert(tf.next);

    Type treti = tf.next;
    if (tf.isRef)
        return fd.isTypeIsolatedIndirect(treti);              // check influence from parameters

    return fd.isTypeIsolated(treti);
}

/********************
 * See if pointers from function parameters, mutable globals, or uplevel functions
 * could leak into type `t`.
 * Params:
 *   t = type to check if it is isolated
 * Returns:
 *   true if `t` is isolated from
 *   any inputs to the function
 */
extern (D) bool isTypeIsolated(FuncDeclaration fd, Type t)
{
    StringTable!Type parentTypes;
    const uniqueTypeID = t.getUniqueID();
    if (uniqueTypeID)
    {
        const cacheResultPtr = uniqueTypeID in fd.isTypeIsolatedCache;
        if (cacheResultPtr !is null)
            return *cacheResultPtr;

        parentTypes._init();
        const isIsolated = fd.isTypeIsolated(t, parentTypes);
        fd.isTypeIsolatedCache[uniqueTypeID] = isIsolated;
        return isIsolated;
    }
    else
    {
        parentTypes._init();
        return fd.isTypeIsolated(t, parentTypes);
    }
}

///ditto
extern (D) bool isTypeIsolated(FuncDeclaration fd, Type t, ref StringTable!Type parentTypes)
{
    //printf("this: %s, isTypeIsolated(t: %s)\n", this.toChars(), t.toChars());

    t = t.baseElemOf();
    switch (t.ty)
    {
        case Tarray:
        case Tpointer:
            return fd.isTypeIsolatedIndirect(t.nextOf()); // go down one level

        case Taarray:
        case Tclass:
            return fd.isTypeIsolatedIndirect(t);

        case Tstruct:
            /* Drill down and check the struct's fields
             */
            auto sym = t.toDsymbol(null).isStructDeclaration();
            const tName = t.toChars.toDString;
            const entry = parentTypes.insert(tName, t);
            if (entry == null)
            {
                //we've already seen this type in a parent, not isolated
                return false;
            }
            foreach (v; sym.fields)
            {
                Type tmi = v.type.addMod(t.mod);
                //printf("\tt = %s, v: %s, vtype: %s,  tmi = %s\n",
                //       t.toChars(), v.toChars(), v.type.toChars(), tmi.toChars());
                if (!fd.isTypeIsolated(tmi, parentTypes))
                    return false;
            }
            return true;

        default:
            return true;
    }
}

/**
 * Check signature of `pragma(printf)` function, print error if invalid.
 *
 * printf/scanf-like functions must be of the form:
 *    extern (C/C++) T printf([parameters...], const(char)* format, ...);
 * or:
 *    extern (C/C++) T vprintf([parameters...], const(char)* format, va_list);
 *
 * Params:
 *      funcdecl = function to check
 *      f = function type
 *      sc = scope
 */
private void checkPrintfScanfSignature(FuncDeclaration funcdecl, TypeFunction f, Scope* sc)
{
    static bool isPointerToChar(Parameter p)
    {
        if (auto tptr = p.type.isTypePointer())
        {
            return tptr.next.ty == Tchar;
        }
        return false;
    }

    bool isVa_list(Parameter p)
    {
        return p.type.equals(target.va_listType(funcdecl.loc, sc));
    }

    const nparams = f.parameterList.length;
    const p = (funcdecl.printf ? Id.printf : Id.scanf).toChars();
    if (!(f.linkage == LINK.c || f.linkage == LINK.cpp))
    {
        .error(funcdecl.loc, "`pragma(%s)` function `%s` must have `extern(C)` or `extern(C++)` linkage,"
            ~" not `extern(%s)`",
            p, funcdecl.toChars(), f.linkage.linkageToChars());
    }
    if (f.parameterList.varargs == VarArg.variadic)
    {
        if (!(nparams >= 1 && isPointerToChar(f.parameterList[nparams - 1])))
        {
            .error(funcdecl.loc, "`pragma(%s)` function `%s` must have"
                ~ " signature `%s %s([parameters...], const(char)*, ...)` not `%s`",
                p, funcdecl.toChars(), f.next.toChars(), funcdecl.toChars(), funcdecl.type.toChars());
        }
    }
    else if (f.parameterList.varargs == VarArg.none)
    {
        if(!(nparams >= 2 && isPointerToChar(f.parameterList[nparams - 2]) &&
            isVa_list(f.parameterList[nparams - 1])))
            .error(funcdecl.loc, "`pragma(%s)` function `%s` must have"~
                " signature `%s %s([parameters...], const(char)*, va_list)`",
                p, funcdecl.toChars(), f.next.toChars(), funcdecl.toChars());
    }
    else
    {
        .error(funcdecl.loc, "`pragma(%s)` function `%s` must have C-style variadic `...` or `va_list` parameter",
            p, funcdecl.toChars());
    }
}

/***************************************************
 * Visit each overloaded function/template in turn, and call dg(s) on it.
 * Exit when no more, or dg(s) returns nonzero.
 *
 * Params:
 *  fstart = symbol to start from
 *  dg = the delegate to be called on the overload
 *  sc = context used to check if symbol is accessible (and therefore visible),
 *       can be null
 *
 * Returns:
 *      ==0     continue
 *      !=0     done (and the return value from the last dg() call)
 */
extern (D) int overloadApply(Dsymbol fstart, scope int delegate(Dsymbol) dg, Scope* sc = null)
{
    Dsymbols visited;

    int overloadApplyRecurse(Dsymbol fstart, scope int delegate(Dsymbol) dg, Scope* sc)
    {
        // Detect cyclic calls.
        if (visited.contains(fstart))
            return 0;
        visited.push(fstart);

        Dsymbol next;
        for (auto d = fstart; d; d = next)
        {
            import dmd.access : checkSymbolAccess;
            if (auto od = d.isOverDeclaration())
            {
                /* The scope is needed here to check whether a function in
                 an overload set was added by means of a private alias (or a
                 selective import). If the scope where the alias is created
                 is imported somewhere, the overload set is visible, but the private
                 alias is not.
                 */
                if (sc)
                {
                    if (checkSymbolAccess(sc, od))
                    {
                        if (int r = overloadApplyRecurse(od.aliassym, dg, sc))
                            return r;
                    }
                }
                else if (int r = overloadApplyRecurse(od.aliassym, dg, sc))
                    return r;
                next = od.overnext;
            }
            else if (auto fa = d.isFuncAliasDeclaration())
            {
                if (fa.hasOverloads)
                {
                    if (int r = overloadApplyRecurse(fa.funcalias, dg, sc))
                        return r;
                }
                else if (auto fd = fa.toAliasFunc())
                {
                    if (int r = dg(fd))
                        return r;
                }
                else
                {
                    .error(d.loc, "%s `%s` is aliased to a function", d.kind, d.toPrettyChars);
                    break;
                }
                next = fa.overnext;
            }
            else if (auto ad = d.isAliasDeclaration())
            {
                if (sc)
                {
                    if (checkSymbolAccess(sc, ad))
                    next = ad.toAlias();
                }
                else
                    next = ad.toAlias();
                if (next == ad)
                    break;
                if (next == fstart)
                    break;
            }
            else if (auto td = d.isTemplateDeclaration())
            {
                if (int r = dg(td))
                    return r;
                next = td.overnext;
            }
            else if (auto fd = d.isFuncDeclaration())
            {
                if (int r = dg(fd))
                    return r;
                next = fd.overnext;
            }
            else if (auto os = d.isOverloadSet())
            {
                foreach (ds; os.a)
                if (int r = dg(ds))
                    return r;
            }
            else
            {
                .error(d.loc, "%s `%s` is aliased to a function", d.kind, d.toPrettyChars);
                break;
                // BUG: should print error message?
            }
        }
        return 0;
    }
    return overloadApplyRecurse(fstart, dg, sc);
}
