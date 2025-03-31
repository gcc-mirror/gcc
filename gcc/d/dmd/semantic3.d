/**
 * Performs the semantic3 stage, which deals with function bodies.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/semantic3.d, _semantic3.d)
 * Documentation:  https://dlang.org/phobos/dmd_semantic3.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/semantic3.d
 */

module dmd.semantic3;

import core.stdc.stdio;
import core.stdc.string;

import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.blockexit;
import dmd.clone;
import dmd.ctorflow;
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
import dmd.location;
import dmd.mtype;
import dmd.nogc;
import dmd.nspace;
import dmd.ob;
import dmd.objc;
import dmd.opover;
import dmd.optimize;
import dmd.parse;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.rmem;
import dmd.rootobject;
import dmd.root.utf;
import dmd.sideeffect;
import dmd.statementsem;
import dmd.staticassert;
import dmd.tokens;
import dmd.semantic2;
import dmd.statement;
import dmd.target;
import dmd.templateparamsem;
import dmd.typesem;
import dmd.visitor;

enum LOG = false;


/*************************************
 * Does semantic analysis on function bodies.
 */
void semantic3(Dsymbol dsym, Scope* sc)
{
    scope v = new Semantic3Visitor(sc);
    dsym.accept(v);
}

private extern(C++) final class Semantic3Visitor : Visitor
{
    alias visit = Visitor.visit;

    Scope* sc;
    this(Scope* sc) scope @safe
    {
        this.sc = sc;
    }

    override void visit(Dsymbol) {}

    override void visit(TemplateInstance tempinst)
    {
        static if (LOG)
        {
            printf("TemplateInstance.semantic3('%s'), semanticRun = %d\n", tempinst.toChars(), tempinst.semanticRun);
        }
        //if (toChars()[0] == 'D') *(char*)0=0;
        if (tempinst.semanticRun >= PASS.semantic3)
            return;
        tempinst.semanticRun = PASS.semantic3;
        if (tempinst.errors || !tempinst.members)
            return;

        TemplateDeclaration tempdecl = tempinst.tempdecl.isTemplateDeclaration();
        assert(tempdecl);

        sc = tempdecl._scope;
        sc = sc.push(tempinst.argsym);
        sc = sc.push(tempinst);
        sc.tinst = tempinst;
        sc.minst = tempinst.minst;

        bool needGagging = tempinst.gagged && !global.gag;
        const olderrors = global.errors;
        const oldGaggedErrors = needGagging ? global.startGagging() : -1;
        /* If this is a gagged instantiation, gag errors.
         * Future optimisation: If the results are actually needed, errors
         * would already be gagged, so we don't really need to run semantic
         * on the members.
         */

        for (size_t i = 0; i < tempinst.members.length; i++)
        {
            Dsymbol s = (*tempinst.members)[i];
            s.semantic3(sc);
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
        if (tmix.semanticRun >= PASS.semantic3)
            return;
        tmix.semanticRun = PASS.semantic3;
        static if (LOG)
        {
            printf("TemplateMixin.semantic3('%s')\n", tmix.toChars());
        }
        if (!tmix.members)
            return;

        sc = sc.push(tmix.argsym);
        sc = sc.push(tmix);

        const olderrors = global.errors;

        for (size_t i = 0; i < tmix.members.length; i++)
        {
            Dsymbol s = (*tmix.members)[i];
            s.semantic3(sc);
        }

        if (global.errors != olderrors)
            errorSupplemental(tmix.loc, "parent scope from here: `mixin %s`", tmix.toChars());

        sc = sc.pop();
        sc.pop();
    }

    override void visit(Module mod)
    {
        //printf("Module::semantic3('%s'): parent = %p\n", toChars(), parent);
        if (mod.semanticRun != PASS.semantic2done)
            return;
        mod.semanticRun = PASS.semantic3;
        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope* sc = Scope.createGlobal(mod, global.errorSink); // create root scope
        //printf("Module = %p\n", sc.scopesym);
        if (mod.members)
        {
            // Pass 3 semantic routines: do initializers and function bodies
            for (size_t i = 0; i < mod.members.length; i++)
            {
                Dsymbol s = (*mod.members)[i];
                //printf("Module %s: %s.semantic3()\n", toChars(), s.toChars());
                s.semantic3(sc);

                mod.runDeferredSemantic2();
            }
        }
        if (mod.userAttribDecl)
        {
            mod.userAttribDecl.semantic3(sc);
        }
        sc = sc.pop();
        sc.pop();
        mod.semanticRun = PASS.semantic3done;
    }

    override void visit(FuncDeclaration funcdecl)
    {
        //printf("FuncDeclaration::semantic3(%s '%s', sc = %p)\n", funcdecl.kind(), funcdecl.toChars(), sc);
        import dmd.timetrace;
        import dmd.root.string : toDString;
        timeTraceBeginEvent(TimeTraceEventType.sema3);
        scope (exit) timeTraceEndEvent(TimeTraceEventType.sema3, funcdecl);

        /* Determine if function should add `return 0;`
         */
        bool addReturn0()
        {
            //printf("addReturn0()\n");
            auto f = funcdecl.type.isTypeFunction();

            // C11 5.1.2.2.3
            if (sc.inCfile && funcdecl.isCMain() && f.next.ty == Tint32)
                return true;

            return f.next.ty == Tvoid && (funcdecl.isMain() || funcdecl.isCMain());
        }

        VarDeclaration _arguments = null;

        if (!funcdecl.parent)
        {
            if (global.errors)
                return;
            //printf("FuncDeclaration::semantic3(%s '%s', sc = %p)\n", kind(), toChars(), sc);
            assert(0);
        }
        if (funcdecl.errors || isError(funcdecl.parent))
        {
            funcdecl.errors = true;

            // Mark that the return type could not be inferred
            if (funcdecl.inferRetType)
            {
                assert(funcdecl.type);
                auto tf = funcdecl.type.isTypeFunction();

                // Only change the return type s.t. other analysis is
                // still possible e.g. missmatched parameter types
                if (tf && !tf.next)
                    tf.next = Type.terror;
            }
            return;
        }
        //printf("FuncDeclaration::semantic3('%s.%s', %p, sc = %p, loc = %s)\n", funcdecl.parent.toChars(), funcdecl.toChars(), funcdecl, sc, funcdecl.loc.toChars());
        //fflush(stdout);
        //printf("storage class = x%x %x\n", sc.stc, storage_class);
        //{ static int x; if (++x == 2) *(char*)0=0; }
        //printf("\tlinkage = %d\n", sc.linkage);

        if (funcdecl.ident == Id.opAssign && !funcdecl.inuse)
        {
            if (funcdecl.storage_class & STC.inference)
            {
                /* https://issues.dlang.org/show_bug.cgi?id=15044
                 * For generated opAssign function, any errors
                 * from its body need to be gagged.
                 */
                const oldErrors = global.startGagging();
                ++funcdecl.inuse;
                funcdecl.semantic3(sc);
                --funcdecl.inuse;
                if (global.endGagging(oldErrors))   // if errors happened
                {
                    // Disable generated opAssign, because some members forbid identity assignment.
                    funcdecl.storage_class |= STC.disable;
                    funcdecl.fbody = null;   // remove fbody which contains the error
                    funcdecl.hasSemantic3Errors = false;
                }
                return;
            }
        }

        //printf(" sc.incontract = %d\n", sc.contract);
        if (funcdecl.semanticRun >= PASS.semantic3)
            return;
        funcdecl.semanticRun = PASS.semantic3;
        funcdecl.hasSemantic3Errors = false;
        funcdecl.saferD = sc.previews.safer;

        if (!funcdecl.type || funcdecl.type.ty != Tfunction)
            return;
        TypeFunction f = cast(TypeFunction)funcdecl.type;
        if (!funcdecl.inferRetType && f.next.ty == Terror)
            return;

        if (!funcdecl.fbody && funcdecl.inferRetType && !f.next)
        {
            .error(funcdecl.loc, "%s `%s` has no function body with return type inference", funcdecl.kind, funcdecl.toPrettyChars);
            return;
        }

        const oldErrors = global.errors;
        auto fds = FuncDeclSem3(funcdecl,sc);

        fds.checkInContractOverrides();

        // Remember whether we need to generate an 'out' contract.
        immutable bool needEnsure = funcdecl.needsFensure();

        if (funcdecl.fbody || funcdecl.frequires || needEnsure)
        {
            /* Symbol table into which we place parameters and nested functions,
             * solely to diagnose name collisions.
             */
            funcdecl.localsymtab = new DsymbolTable();

            // Establish function scope
            auto ss = new ScopeDsymbol(funcdecl.loc, null);
            // find enclosing scope symbol, might skip symbol-less CTFE and/or FuncExp scopes
            ss.parent = sc.inner().scopesym;
            ss.endlinnum = funcdecl.endloc.linnum;
            Scope* sc2 = sc.push(ss);
            sc2.func = funcdecl;
            sc2.parent = funcdecl;
            sc2.ctorflow.callSuper = CSX.none;
            sc2.sbreak = null;
            sc2.scontinue = null;
            sc2.switchStatement = null;
            sc2.fes = funcdecl.fes;
            sc2.linkage = funcdecl.isCsymbol() ? LINK.c : LINK.d;
            sc2.stc &= STC.flowThruFunction;
            sc2.visibility = Visibility(Visibility.Kind.public_);
            sc2.explicitVisibility = 0;
            sc2.aligndecl = null;
            if (funcdecl.ident != Id.require && funcdecl.ident != Id.ensure)
            {
                sc2.copyFlagsFrom(sc);
                sc2.contract = Contract.none;
            }
            sc2.tryFinally = null;
            sc2.scopeGuard = null;
            sc2.inLoop = false;
            sc2.inDefaultArg = false;
            sc2.userAttribDecl = null;
            if (sc2.intypeof == 1)
                sc2.intypeof = 2;
            sc2.ctorflow.fieldinit = null;

            /* Note: When a lambda is defined immediately under aggregate member
             * scope, it should be contextless due to prevent interior pointers.
             * e.g.
             *      // dg points 'this' - its interior pointer
             *      class C { int x; void delegate() dg = (){ this.x = 1; }; }
             *
             * However, lambdas could be used inside typeof, in order to check
             * some expressions validity at compile time. For such case the lambda
             * body can access aggregate instance members.
             * e.g.
             *      class C { int x; static assert(is(typeof({ this.x = 1; }))); }
             *
             * To properly accept it, mark these lambdas as member functions.
             */
            if (auto fld = funcdecl.isFuncLiteralDeclaration())
            {
                if (auto ad = funcdecl.isMember2())
                {
                    if (!sc.intypeof)
                    {
                        if (fld.tok == TOK.delegate_)
                            .error(funcdecl.loc, "%s `%s` cannot be %s members", funcdecl.kind, funcdecl.toErrMsg, ad.kind());
                        else
                            fld.tok = TOK.function_;
                    }
                    else
                    {
                        if (fld.tok != TOK.function_)
                            fld.tok = TOK.delegate_;
                    }
                }
            }

            declareThis(funcdecl, sc2);

            // Reverts: https://issues.dlang.org/show_bug.cgi?id=5710
            // No compiler supports this, and there was never any spec for it.
            // @@@DEPRECATED_2.116@@@
            // Deprecated in 2.096, can be made an error in 2.116.
            // The deprecation period is longer than usual as dual-context
            // functions may be widely used by dmd-compiled projects.
            // It also gives more time for the implementation of dual-context
            // functions to be reworked as a frontend-only feature.
            if (funcdecl.hasDualContext())
            {
                .deprecation(funcdecl.loc, "%s `%s` function requires a dual-context, which is deprecated", funcdecl.kind, funcdecl.toPrettyChars);
                if (auto ti = sc2.parent ? sc2.parent.isInstantiated() : null)
                    ti.printInstantiationTrace(Classification.deprecation);
            }

            //printf("[%s] ad = %p vthis = %p\n", loc.toChars(), ad, vthis);
            //if (vthis) printf("\tvthis.type = %s\n", vthis.type.toChars());

            // Declare hidden variable _arguments[] and _argptr
            if (f.parameterList.varargs == VarArg.variadic)
            {
                if (f.linkage == LINK.d)
                {
                    // Variadic arguments depend on Typeinfo being defined.
                    if (!global.params.useTypeInfo || !Type.dtypeinfo || !Type.typeinfotypelist)
                    {
                        if (!global.params.useTypeInfo)
                        {
                            version (IN_GCC)
                                .error(funcdecl.loc, "%s `%s` D-style variadic functions cannot be used with `-fno-rtti`", funcdecl.kind, funcdecl.toPrettyChars);
                            else
                                .error(funcdecl.loc, "%s `%s` D-style variadic functions cannot be used with -betterC", funcdecl.kind, funcdecl.toPrettyChars);
                        }
                        else if (!Type.typeinfotypelist)
                            .error(funcdecl.loc, "%s `%s` `object.TypeInfo_Tuple` could not be found, but is implicitly used in D-style variadic functions", funcdecl.kind, funcdecl.toPrettyChars);
                        else
                            .error(funcdecl.loc, "%s `%s` `object.TypeInfo` could not be found, but is implicitly used in D-style variadic functions", funcdecl.kind, funcdecl.toPrettyChars);
                        funcdecl.errors = true;
                    }
                }

                if (!funcdecl.errors && f.linkage == LINK.d)
                {
                    // Declare _arguments[]
                    funcdecl.v_arguments = new VarDeclaration(funcdecl.loc, Type.typeinfotypelist.type, Id._arguments_typeinfo, null);
                    funcdecl.v_arguments.storage_class |= STC.temp | STC.parameter;
                    funcdecl.v_arguments.dsymbolSemantic(sc2);
                    sc2.insert(funcdecl.v_arguments);
                    funcdecl.v_arguments.parent = funcdecl;

                    //Type t = Type.dtypeinfo.type.constOf().arrayOf();
                    Type t = Type.dtypeinfo.type.arrayOf();
                    _arguments = new VarDeclaration(funcdecl.loc, t, Id._arguments, null);
                    _arguments.storage_class |= STC.temp;
                    _arguments.dsymbolSemantic(sc2);
                    sc2.insert(_arguments);
                    _arguments.parent = funcdecl;
                }
                if (!funcdecl.errors && (f.linkage == LINK.d || f.parameterList.length))
                {
                    // Declare _argptr
                    Type t = target.va_listType(funcdecl.loc, sc);
                    // Init is handled in FuncDeclaration_toObjFile
                    funcdecl.v_argptr = new VarDeclaration(funcdecl.loc, t, Id._argptr, new VoidInitializer(funcdecl.loc));
                    funcdecl.v_argptr.storage_class |= STC.temp;
                    funcdecl.v_argptr.dsymbolSemantic(sc2);
                    sc2.insert(funcdecl.v_argptr);
                    funcdecl.v_argptr.parent = funcdecl;
                }
            }

            /* Declare all the function parameters as variables
             * and install them in parameters[]
             */
            if (const nparams = f.parameterList.length)
            {
                /* parameters[] has all the tuples removed, as the back end
                 * doesn't know about tuples
                 */
                funcdecl.parameters = new VarDeclarations();
                funcdecl.parameters.reserve(nparams);
                foreach (i, fparam; f.parameterList)
                {
                    Identifier id = fparam.ident;
                    STC stc = STC.none;
                    if (!id)
                    {
                        /* Generate identifier for un-named parameter,
                         * because we need it later on.
                         */
                        fparam.ident = id = Identifier.generateId("__param_", i);
                        stc |= STC.temp;
                    }
                    Type vtype = fparam.type;
                    auto v = new VarDeclaration(fparam.loc, vtype, id, null);
                    //printf("declaring parameter %s of type %s\n", v.toChars(), v.type.toChars());
                    stc |= STC.parameter;
                    if (f.parameterList.varargs == VarArg.typesafe && i + 1 == nparams)
                    {
                        stc |= STC.variadic;
                    }

                    stc |= fparam.storageClass & (STC.IOR | STC.return_ | STC.scope_ | STC.lazy_ | STC.final_ | STC.TYPECTOR | STC.nodtor | STC.returnScope | STC.register);
                    v.storage_class = stc;
                    v.dsymbolSemantic(sc2);
                    if (!sc2.insert(v))
                    {
                        .error(funcdecl.loc, "%s `%s` parameter `%s.%s` is already defined", funcdecl.kind, funcdecl.toPrettyChars, funcdecl.toChars(), v.toChars());
                        funcdecl.errors = true;
                    }
                    else
                        funcdecl.parameters.push(v);
                    funcdecl.localsymtab.insert(v);
                    v.parent = funcdecl;
                    if (fparam.userAttribDecl)
                        v.userAttribDecl = fparam.userAttribDecl;
                }
            }

            // Declare the tuple symbols and put them in the symbol table,
            // but not in parameters[].
            if (f.parameterList.parameters)
            foreach (fparam; *f.parameterList.parameters)
            {
                if (!fparam.ident)
                    continue; // never used, so ignore
                // expand any tuples
                if (fparam.type.ty != Ttuple)
                    continue;

                TypeTuple t = cast(TypeTuple)fparam.type;
                size_t dim = Parameter.dim(t.arguments);
                auto exps = new Objects(dim);
                foreach (j; 0 .. dim)
                {
                    Parameter narg = Parameter.getNth(t.arguments, j);
                    assert(narg.ident);
                    Dsymbol pscopesym;
                    VarDeclaration v = sc2.search(Loc.initial, narg.ident, pscopesym).isVarDeclaration();
                    assert(v);
                    (*exps)[j] = new VarExp(v.loc, v);
                }
                assert(fparam.ident);
                auto v = new TupleDeclaration(funcdecl.loc, fparam.ident, exps);
                //printf("declaring tuple %s\n", v.toChars());
                v.isexp = true;
                if (!sc2.insert(v))
                    .error(funcdecl.loc, "%s `%s` parameter `%s.%s` is already defined", funcdecl.kind, funcdecl.toPrettyChars, funcdecl.toChars(), v.toChars());
                funcdecl.localsymtab.insert(v);
                v.parent = funcdecl;
            }

            // Precondition invariant
            Statement fpreinv = null;
            if (funcdecl.addPreInvariant())
            {
                if (Expression e = addInvariant(funcdecl.isThis(), funcdecl.vthis))
                    fpreinv = new ExpStatement(Loc.initial, e);
            }

            // Postcondition invariant
            Statement fpostinv = null;
            if (funcdecl.addPostInvariant())
            {
                if (Expression e = addInvariant(funcdecl.isThis(), funcdecl.vthis))
                    fpostinv = new ExpStatement(Loc.initial, e);
            }

            // Pre/Postcondition contract
            if (!funcdecl.fbody)
                funcdecl.buildEnsureRequire();

            Scope* scout = null;
            if (needEnsure || funcdecl.addPostInvariant())
            {
                /* https://issues.dlang.org/show_bug.cgi?id=3657
                 * Set the correct end line number for fensure scope.
                 */
                uint fensure_endlin = funcdecl.endloc.linnum;
                if (funcdecl.fensure)
                    if (auto s = funcdecl.fensure.isScopeStatement())
                        fensure_endlin = s.endloc.linnum;

                if ((needEnsure && global.params.useOut == CHECKENABLE.on) || fpostinv)
                {
                    funcdecl.returnLabel = funcdecl.searchLabel(Id.returnLabel, Loc.initial);
                }

                // scope of out contract (need for vresult.semantic)
                auto sym = new ScopeDsymbol(funcdecl.loc, null);
                sym.parent = sc2.scopesym;
                sym.endlinnum = fensure_endlin;
                scout = sc2.push(sym);
            }

            if (funcdecl.fbody)
            {
                auto sym = new ScopeDsymbol(funcdecl.loc, null);
                sym.parent = sc2.scopesym;
                sym.endlinnum = funcdecl.endloc.linnum;
                sc2 = sc2.push(sym);

                auto ad2 = funcdecl.isMemberLocal();

                /* If this is a class constructor
                 */
                if (ad2 && funcdecl.isCtorDeclaration())
                {
                    sc2.ctorflow.allocFieldinit(ad2.fields.length);
                    foreach (v; ad2.fields)
                    {
                        v.ctorinit = 0;
                    }
                }

                bool inferRef = (f.isRef && (funcdecl.storage_class & STC.auto_));

                funcdecl.fbody = funcdecl.fbody.statementSemantic(sc2);
                if (!funcdecl.fbody)
                    funcdecl.fbody = new CompoundStatement(Loc.initial, new Statements());

                if (funcdecl.isNaked())
                {
                    fpreinv = null;         // can't accommodate with no stack frame
                    fpostinv = null;
                }

                assert(funcdecl.type == f || (funcdecl.type.ty == Tfunction && f.purity == PURE.impure && (cast(TypeFunction)funcdecl.type).purity >= PURE.fwdref));
                f = cast(TypeFunction)funcdecl.type;

                if (funcdecl.inferRetType)
                {
                    // If no return type inferred yet, then infer a void
                    if (!f.next)
                        f.next = Type.tvoid;
                    if (f.checkRetType(funcdecl.loc))
                        funcdecl.fbody = new ErrorStatement();
                    else
                        funcdecl.checkMain(); // Check main() parameters and return type
                }

                if (f.next !is null)
                    f.next.checkComplexTransition(funcdecl.loc, sc);

                if (funcdecl.returns && !funcdecl.fbody.isErrorStatement())
                {
                    for (size_t i = 0; i < funcdecl.returns.length;)
                    {
                        Expression exp = (*funcdecl.returns)[i].exp;
                        if (exp.op == EXP.variable && (cast(VarExp)exp).var == funcdecl.vresult)
                        {
                            if (addReturn0())
                                exp.type = Type.tint32;
                            else
                                exp.type = f.next;
                            // Remove `return vresult;` from returns
                            funcdecl.returns.remove(i);
                            continue;
                        }
                        if (inferRef && f.isRef && !exp.type.constConv(f.next)) // https://issues.dlang.org/show_bug.cgi?id=13336
                            f.isRef = false;
                        i++;
                    }
                }
                if (f.isRef) // Function returns a reference
                {
                    if (funcdecl.storage_class & STC.auto_)
                        funcdecl.storage_class &= ~STC.auto_;
                }

                // handle NRVO
                if (!target.isReturnOnStack(f, funcdecl.needThis()) || !funcdecl.checkNRVO())
                    funcdecl.isNRVO = false;

                if (funcdecl.fbody.isErrorStatement())
                {
                }
                else if (funcdecl.isStaticCtorDeclaration())
                {
                    /* It's a static constructor. Ensure that all
                     * ctor consts were initialized.
                     */
                    ScopeDsymbol pd = funcdecl.toParent().isScopeDsymbol();
                    for (size_t i = 0; i < pd.members.length; i++)
                    {
                        Dsymbol s = (*pd.members)[i];
                        s.checkCtorConstInit();
                    }
                }
                else if (ad2 && funcdecl.isCtorDeclaration())
                {
                    ClassDeclaration cd = ad2.isClassDeclaration();

                    // Verify that all the ctorinit fields got initialized
                    if (!(sc2.ctorflow.callSuper & CSX.this_ctor))
                    {
                        foreach (i, v; ad2.fields)
                        {
                            if (v.isThisDeclaration())
                                continue;
                            if (v.ctorinit == 0)
                            {
                                /* Current bugs in the flow analysis:
                                 * 1. union members should not produce error messages even if
                                 *    not assigned to
                                 * 2. structs should recognize delegating opAssign calls as well
                                 *    as delegating calls to other constructors
                                 */
                                if (v.isCtorinit() && !v.type.isMutable() && cd)
                                    .error(funcdecl.loc, "%s `%s` missing initializer for %s field `%s`", funcdecl.kind, funcdecl.toPrettyChars, MODtoChars(v.type.mod), v.toChars());
                                else if (v.storage_class & STC.nodefaultctor)
                                    error(funcdecl.loc, "field `%s` must be initialized in constructor", v.toChars());
                                else if (v.type.needsNested())
                                    error(funcdecl.loc, "field `%s` must be initialized in constructor, because it is nested struct", v.toChars());
                            }
                            else
                            {
                                bool mustInit = (v.storage_class & STC.nodefaultctor || v.type.needsNested());
                                if (mustInit && !(sc2.ctorflow.fieldinit[i].csx & CSX.this_ctor))
                                {
                                    .error(funcdecl.loc, "%s `%s` field `%s` must be initialized but skipped", funcdecl.kind, funcdecl.toPrettyChars, v.toChars());
                                }
                            }
                        }
                    }
                    sc2.ctorflow.freeFieldinit();

                    if (cd && !(sc2.ctorflow.callSuper & (CSX.any_ctor | CSX.halt)) && cd.baseClass && cd.baseClass.ctor)
                    {
                        sc2.ctorflow.callSuper = CSX.none;

                        // Insert implicit super() at start of fbody
                        Type tthis = ad2.type.addMod(funcdecl.vthis.type.mod);
                        FuncDeclaration fd = resolveFuncCall(Loc.initial, sc2, cd.baseClass.ctor, null, tthis, ArgumentList(), FuncResolveFlag.quiet);
                        if (!fd)
                        {
                            .error(funcdecl.loc, "%s `%s` no match for implicit `super()` call in constructor", funcdecl.kind, funcdecl.toPrettyChars);
                        }
                        else if (fd.storage_class & STC.disable)
                        {
                            .error(funcdecl.loc, "%s `%s` cannot call `super()` implicitly because it is annotated with `@disable`", funcdecl.kind, funcdecl.toPrettyChars);
                        }
                        else
                        {
                            Expression e1 = new SuperExp(Loc.initial);
                            Expression e = new CallExp(Loc.initial, e1);
                            e = e.expressionSemantic(sc2);
                            Statement s = new ExpStatement(Loc.initial, e);
                            funcdecl.fbody = new CompoundStatement(Loc.initial, s, funcdecl.fbody);
                        }
                    }
                    //printf("ctorflow.callSuper = x%x\n", sc2.ctorflow.callSuper);
                }

                /* https://issues.dlang.org/show_bug.cgi?id=17502
                 * Wait until after the return type has been inferred before
                 * generating the contracts for this function, and merging contracts
                 * from overrides.
                 *
                 * https://issues.dlang.org/show_bug.cgi?id=17893
                 * However should take care to generate this before inferered
                 * function attributes are applied, such as 'nothrow'.
                 *
                 * This was originally at the end of the first semantic pass, but
                 * required a fix-up to be done here for the '__result' variable
                 * type of __ensure() inside auto functions, but this didn't work
                 * if the out parameter was implicit.
                 */
                funcdecl.buildEnsureRequire();

                // Check for errors related to 'nothrow'.
                const blockexit = funcdecl.fbody.blockExit(funcdecl, f.isNothrow ? global.errorSink : null);
                if (f.isNothrow && blockexit & BE.throw_)
                    error(funcdecl.loc, "%s `%s` may throw but is marked as `nothrow`", funcdecl.kind(), funcdecl.toPrettyChars());

                if (!(blockexit & (BE.throw_ | BE.halt) || funcdecl.hasCatches))
                {
                    /* Don't generate unwind tables for this function
                     * https://issues.dlang.org/show_bug.cgi?id=17997
                     */
                    funcdecl.hasNoEH = true;
                }

                if (funcdecl.nothrowInprocess)
                {
                    if (funcdecl.type == f)
                        f = cast(TypeFunction)f.copy();
                    f.isNothrow = !(blockexit & BE.throw_);
                }

                if (funcdecl.fbody.isErrorStatement())
                {
                }
                else if (ad2 && funcdecl.isCtorDeclaration())
                {
                    /* Append:
                     *  return this;
                     * to function body
                     */
                    if (blockexit & BE.fallthru)
                    {
                        Statement s = new ReturnStatement(funcdecl.loc, null);
                        s = s.statementSemantic(sc2);
                        funcdecl.fbody = new CompoundStatement(funcdecl.loc, funcdecl.fbody, s);
                        funcdecl.hasMultipleReturnExp = funcdecl.hasReturnExp;
                        funcdecl.hasReturnExp = true;
                    }
                }
                else if (funcdecl.fes)
                {
                    // For foreach(){} body, append a return 0;
                    if (blockexit & BE.fallthru)
                    {
                        Expression e = IntegerExp.literal!0;
                        Statement s = new ReturnStatement(Loc.initial, e);
                        funcdecl.fbody = new CompoundStatement(Loc.initial, funcdecl.fbody, s);
                        funcdecl.hasMultipleReturnExp = funcdecl.hasReturnExp;
                        funcdecl.hasReturnExp = true;
                    }
                    assert(!funcdecl.returnLabel);
                }
                else if (f.next.toBasetype().ty == Tnoreturn)
                {
                    // Fallthrough despite being declared as noreturn? return is already rejected when evaluating the ReturnStatement
                    if (blockexit & BE.fallthru)
                    {
                        .error(funcdecl.loc, "%s `%s` is typed as `%s` but does return", funcdecl.kind, funcdecl.toPrettyChars, f.next.toChars());
                        funcdecl.loc.errorSupplemental("`noreturn` functions must either throw, abort or loop indefinitely");
                    }
                }
                else
                {
                    if ((blockexit & BE.fallthru) && f.next.ty != Tvoid && !funcdecl.hasInlineAsm && !sc.inCfile)
                    {
                        if (!funcdecl.hasReturnExp)
                            .error(funcdecl.loc, "%s `%s` has no `return` statement, but is expected to return a value of type `%s`", funcdecl.kind, funcdecl.toPrettyChars, f.next.toChars());
                        else
                            .error(funcdecl.loc, "%s `%s` no `return exp;` or `assert(0);` at end of function", funcdecl.kind, funcdecl.toPrettyChars);
                    }
                }

                if (funcdecl.returns)
                {
                    bool implicit0 = addReturn0();
                    Type tret = implicit0 ? Type.tint32 : f.next;
                    assert(tret.ty != Tvoid);
                    if (funcdecl.vresult || funcdecl.returnLabel)
                        funcdecl.buildResultVar(scout ? scout : sc2, tret);

                    /* Cannot move this loop into NrvoWalker, because
                     * returns[i] may be in the nested delegate for foreach-body.
                     */
                    for (size_t i = 0; i < funcdecl.returns.length; i++)
                    {
                        ReturnStatement rs = (*funcdecl.returns)[i];
                        Expression exp = rs.exp;
                        if (exp.op == EXP.error)
                            continue;
                        if (tret.ty == Terror)
                        {
                            // https://issues.dlang.org/show_bug.cgi?id=13702
                            exp = checkGC(sc2, exp);
                            continue;
                        }

                        /* If the expression in the return statement (exp) cannot be implicitly
                         * converted to the return type (tret) of the function and if the
                         * type of the expression is type isolated, then it may be possible
                         * that a promotion to `immutable` or `inout` (through a cast) will
                         * match the return type.
                         */
                        if (!exp.implicitConvTo(tret) && funcdecl.isTypeIsolated(exp.type))
                        {
                            /* https://issues.dlang.org/show_bug.cgi?id=20073
                             *
                             * The problem is that if the type of the returned expression (exp.type)
                             * is an aggregated declaration with an alias this, the alias this may be
                             * used for the conversion testing without it being an isolated type.
                             *
                             * To make sure this does not happen, we can test here the implicit conversion
                             * only for the aggregated declaration type by using `implicitConvToWithoutAliasThis`.
                             * The implicit conversion with alias this is taken care of later.
                             */
                            AggregateDeclaration aggDecl = isAggregate(exp.type);
                            TypeStruct tstruct;
                            TypeClass tclass;
                            bool hasAliasThis;
                            if (aggDecl && aggDecl.aliasthis)
                            {
                                hasAliasThis = true;
                                tclass = exp.type.isTypeClass();
                                if (!tclass)
                                    tstruct = exp.type.isTypeStruct();
                                assert(tclass || tstruct);
                            }
                            if (hasAliasThis)
                            {
                                if (tclass)
                                {
                                    if ((cast(TypeClass)(exp.type.immutableOf())).implicitConvToWithoutAliasThis(tret))
                                        exp = exp.castTo(sc2, exp.type.immutableOf());
                                    else if ((cast(TypeClass)(exp.type.wildOf())).implicitConvToWithoutAliasThis(tret))
                                        exp = exp.castTo(sc2, exp.type.wildOf());
                                }
                                else
                                {
                                    if ((cast(TypeStruct)exp.type.immutableOf()).implicitConvToWithoutAliasThis(tret))
                                        exp = exp.castTo(sc2, exp.type.immutableOf());
                                    else if ((cast(TypeStruct)exp.type.immutableOf()).implicitConvToWithoutAliasThis(tret))
                                        exp = exp.castTo(sc2, exp.type.wildOf());
                                }
                            }
                            else
                            {
                                if (exp.type.immutableOf().implicitConvTo(tret))
                                    exp = exp.castTo(sc2, exp.type.immutableOf());
                                else if (exp.type.wildOf().implicitConvTo(tret))
                                    exp = exp.castTo(sc2, exp.type.wildOf());
                            }
                        }

                        // Function returns a reference
                        if (f.isRef)
                        {
                            if (!MODimplicitConv(exp.type.mod, tret.mod) && !tret.isTypeSArray())
                                error(exp.loc, "expression `%s` of type `%s` is not implicitly convertible to return type `ref %s`",
                                      exp.toChars(), exp.type.toChars(), tret.toChars());
                            else
                                exp = exp.implicitCastTo(sc2, tret);

                            exp = exp.toLvalue(sc2, "`ref` return");
                            checkReturnEscapeRef(*sc2, exp, false);
                            exp = exp.optimize(WANTvalue, /*keepLvalue*/ true);
                        }
                        else
                        {
                            // if a copy constructor is present, the return type conversion will be handled by it
                            const hasCopyCtor = exp.type.ty == Tstruct && (cast(TypeStruct)exp.type).sym.hasCopyCtor;
                            if (!hasCopyCtor || !exp.isLvalue())
                            {
                                const errors = global.startGagging();
                                auto implicitlyCastedExp = exp.implicitCastTo(sc2, tret);
                                global.endGagging(errors);

                                // <https://github.com/dlang/dmd/issues/20888>
                                if (implicitlyCastedExp.isErrorExp())
                                {
                                    auto types = toAutoQualChars(exp.type, tret);
                                    error(
                                        exp.loc,
                                        "return value `%s` of type `%s` does not match return type `%s`"
                                        ~ ", and cannot be implicitly converted",
                                        exp.toErrMsg(),
                                        types[0],
                                        types[1],
                                    );

                                    if (const func = exp.type.isFunction_Delegate_PtrToFunction())
                                        if (func.next.equals(tret))
                                            errorSupplemental(
                                                exp.loc,
                                                "Did you intend to call the %s?",
                                                (exp.type.isPtrToFunction())
                                                    ? "function pointer"
                                                    : exp.type.kind
                                            );
                                }

                                exp = implicitlyCastedExp;
                            }

                            exp = exp.optimize(WANTvalue);

                            /* https://issues.dlang.org/show_bug.cgi?id=10789
                             * If NRVO is not possible, all returned lvalues should call their postblits.
                             */
                            if (!funcdecl.isNRVO())
                                exp = doCopyOrMove(sc2, exp, f.next, true, true);

                            if (tret.hasPointers())
                                checkReturnEscape(*sc2, exp, false);
                        }

                        exp = checkGC(sc2, exp);

                        if (funcdecl.vresult)
                        {
                            // Create: return vresult = exp;
                            exp = new BlitExp(rs.loc, funcdecl.vresult, exp);
                            exp.type = funcdecl.vresult.type;

                            if (rs.caseDim)
                                exp = Expression.combine(exp, new IntegerExp(rs.caseDim));
                        }
                        else if (funcdecl.tintro && !tret.equals(funcdecl.tintro.nextOf()))
                        {
                            exp = exp.implicitCastTo(sc2, funcdecl.tintro.nextOf());
                        }
                        rs.exp = exp;
                    }
                }
                if (funcdecl.nrvo_var || funcdecl.returnLabel)
                {
                    scope NrvoWalker nw = new NrvoWalker();
                    nw.fd = funcdecl;
                    nw.sc = sc2;
                    nw.visitStmt(funcdecl.fbody);
                }

                sc2 = sc2.pop();
            }

            if (sc.previews.inclusiveInContracts)
            {
                funcdecl.frequire = funcdecl.mergeFrequireInclusivePreview(
                    funcdecl.frequire, funcdecl.fdrequireParams);
            }
            else
            {
                funcdecl.frequire = funcdecl.mergeFrequire(funcdecl.frequire, funcdecl.fdrequireParams);
            }
            funcdecl.fensure = funcdecl.mergeFensure(funcdecl.fensure, Id.result, funcdecl.fdensureParams);

            Statement freq = funcdecl.frequire;
            Statement fens = funcdecl.fensure;

            /* Do the semantic analysis on the [in] preconditions and
             * [out] postconditions.
             */
            immutable bool isNothrow = f.isNothrow && !funcdecl.nothrowInprocess;
            if (freq)
            {
                /* frequire is composed of the [in] contracts
                 */
                auto sym = new ScopeDsymbol(funcdecl.loc, null);
                sym.parent = sc2.scopesym;
                sym.endlinnum = funcdecl.endloc.linnum;
                sc2 = sc2.push(sym);
                sc2.contract = Contract.require;

                // BUG: need to error if accessing out parameters
                // BUG: need to disallow returns
                // BUG: verify that all in and ref parameters are read
                freq = freq.statementSemantic(sc2);

                const blockExit = freq.blockExit(funcdecl, null);
                if (blockExit & BE.throw_)
                {
                    if (isNothrow)
                        error(funcdecl.loc, "`%s`: `in` contract may throw but function is marked as `nothrow`",
                            funcdecl.toPrettyChars());
                    else if (funcdecl.nothrowInprocess)
                        f.isNothrow = false;
                }

                funcdecl.hasNoEH = false;

                sc2 = sc2.pop();

                if (global.params.useIn == CHECKENABLE.off)
                    freq = null;
            }

            if (fens)
            {
                /* fensure is composed of the [out] contracts
                 */
                if (f.next.ty == Tvoid && funcdecl.fensures)
                {
                    foreach (e; *funcdecl.fensures)
                    {
                        if (e.id)
                        {
                            .error(e.ensure.loc, "%s `%s` `void` functions have no result", funcdecl.kind, funcdecl.toPrettyChars);
                            //fens = null;
                        }
                    }
                }

                sc2 = scout; //push
                sc2.contract = Contract.ensure;

                // BUG: need to disallow returns and throws

                if (funcdecl.fensure && f.next.ty != Tvoid)
                    funcdecl.buildResultVar(scout, f.next);

                fens = fens.statementSemantic(sc2);

                const blockExit = fens.blockExit(funcdecl, null);
                if (blockExit & BE.throw_)
                {
                    if (isNothrow)
                        error(funcdecl.loc, "`%s`: `out` contract may throw but function is marked as `nothrow`",
                            funcdecl.toPrettyChars());
                    else if (funcdecl.nothrowInprocess)
                        f.isNothrow = false;
                }

                funcdecl.hasNoEH = false;

                sc2 = sc2.pop();

                if (global.params.useOut == CHECKENABLE.off)
                    fens = null;
            }
            if (funcdecl.fbody && funcdecl.fbody.isErrorStatement())
            {
            }
            else
            {
                auto a = new Statements();
                // Merge in initialization of 'out' parameters
                if (funcdecl.parameters)
                {
                    for (size_t i = 0; i < funcdecl.parameters.length; i++)
                    {
                        VarDeclaration v = (*funcdecl.parameters)[i];
                        if (v.storage_class & STC.out_)
                        {
                            if (!v._init)
                            {
                                .error(v.loc, "%s `%s` zero-length `out` parameters are not allowed.", v.kind, v.toPrettyChars);
                                return;
                            }
                            ExpInitializer ie = v._init.isExpInitializer();
                            assert(ie);
                            if (auto iec = ie.exp.isConstructExp())
                            {
                                // construction occurred in parameter processing
                                auto ec = new AssignExp(iec.loc, iec.e1, iec.e2);
                                ec.type = iec.type;
                                ie.exp = ec;
                            }
                            a.push(new ExpStatement(Loc.initial, ie.exp));
                        }
                    }
                }

                if (_arguments)
                {
                    /* Advance to elements[] member of TypeInfo_Tuple with:
                     *  _arguments = v_arguments.elements;
                     */
                    Expression e = new VarExp(Loc.initial, funcdecl.v_arguments);
                    e = new DotIdExp(Loc.initial, e, Id.elements);
                    e = new ConstructExp(Loc.initial, _arguments, e);
                    e = e.expressionSemantic(sc2);

                    _arguments._init = new ExpInitializer(Loc.initial, e);
                    auto de = new DeclarationExp(Loc.initial, _arguments);
                    a.push(new ExpStatement(Loc.initial, de));
                }

                // Merge contracts together with body into one compound statement

                if (freq || fpreinv)
                {
                    if (!freq)
                        freq = fpreinv;
                    else if (fpreinv)
                        freq = new CompoundStatement(Loc.initial, freq, fpreinv);

                    a.push(freq);
                }

                if (funcdecl.fbody)
                    a.push(funcdecl.fbody);

                if (fens || fpostinv)
                {
                    if (!fens)
                        fens = fpostinv;
                    else if (fpostinv)
                        fens = new CompoundStatement(Loc.initial, fpostinv, fens);

                    auto ls = new LabelStatement(Loc.initial, Id.returnLabel, fens);
                    funcdecl.returnLabel.statement = ls;
                    a.push(funcdecl.returnLabel.statement);

                    if (f.next.ty != Tvoid && funcdecl.vresult)
                    {
                        // Create: return vresult;
                        Expression e = new VarExp(Loc.initial, funcdecl.vresult);
                        if (funcdecl.tintro)
                        {
                            e = e.implicitCastTo(sc, funcdecl.tintro.nextOf());
                            e = e.expressionSemantic(sc);
                        }
                        auto s = new ReturnStatement(Loc.initial, e);
                        a.push(s);
                    }
                }
                if (addReturn0())
                {
                    // Add a return 0; statement
                    Statement s = new ReturnStatement(Loc.initial, IntegerExp.literal!0);
                    a.push(s);
                }

                Statement sbody = new CompoundStatement(Loc.initial, a);

                /* Append destructor calls for parameters as finally blocks.
                 */
                if (funcdecl.parameters)
                {
                    // check if callee destroys arguments
                    const bool paramsNeedDtor = target.isCalleeDestroyingArgs(f);

                    foreach (v; *funcdecl.parameters)
                    {
                        if (v.isReference() || (v.storage_class & STC.lazy_))
                            continue;
                        if (!v.needsScopeDtor())
                            continue;
                        v.storage_class |= STC.nodtor;
                        if (!paramsNeedDtor)
                            continue;

                        // same with ExpStatement.scopeCode()
                        Statement s = new DtorExpStatement(Loc.initial, v.edtor, v);

                        s = s.statementSemantic(sc2);

                        const blockexit = s.blockExit(funcdecl, isNothrow ? global.errorSink : null);
                        if (blockexit & BE.throw_)
                        {
                            funcdecl.hasNoEH = false;
                            if (isNothrow)
                                error(funcdecl.loc, "%s `%s` may throw but is marked as `nothrow`", funcdecl.kind(), funcdecl.toPrettyChars());
                            else if (funcdecl.nothrowInprocess)
                                f.isNothrow = false;
                        }

                        if (sbody.blockExit(funcdecl, f.isNothrow ? global.errorSink : null) == BE.fallthru)
                            sbody = new CompoundStatement(Loc.initial, sbody, s);
                        else
                            sbody = new TryFinallyStatement(Loc.initial, sbody, s);
                    }
                }
                // from this point on all possible 'throwers' are checked
                funcdecl.nothrowInprocess = false;

                if (funcdecl.isSynchronized())
                {
                    /* Wrap the entire function body in a synchronized statement
                     */
                    if (ClassDeclaration cd = funcdecl.toParentDecl().isClassDeclaration())
                    {
                        if (target.libraryObjectMonitors(funcdecl, sbody))
                        {
                            Expression vsync;
                            if (funcdecl.isStatic())
                            {
                                // The monitor is in the ClassInfo
                                vsync = new DotIdExp(funcdecl.loc, symbolToExp(cd, funcdecl.loc, sc2, false), Id.classinfo);
                            }
                            else
                            {
                                // 'this' is the monitor
                                vsync = new VarExp(funcdecl.loc, funcdecl.vthis);
                                if (funcdecl.hasDualContext())
                                {
                                    vsync = new PtrExp(funcdecl.loc, vsync);
                                    vsync = new IndexExp(funcdecl.loc, vsync, IntegerExp.literal!0);
                                }
                            }
                            sbody = new PeelStatement(sbody); // don't redo semantic()
                            sbody = new SynchronizedStatement(funcdecl.loc, vsync, sbody);
                            sbody = sbody.statementSemantic(sc2);
                        }
                    }
                    else
                    {
                        .error(funcdecl.loc, "%s `%s` synchronized function `%s` must be a member of a class", funcdecl.kind, funcdecl.toPrettyChars, funcdecl.toChars());
                    }
                }

                // If declaration has no body, don't set sbody to prevent incorrect codegen.
                if (funcdecl.fbody || funcdecl.allowsContractWithoutBody())
                    funcdecl.fbody = sbody;
            }

            // Check for undefined labels
            if (funcdecl.labtab)
                foreach (keyValue; funcdecl.labtab.tab.asRange)
                {
                    //printf("  KV: %s = %s\n", keyValue.key.toChars(), keyValue.value.toChars());
                    LabelDsymbol label = cast(LabelDsymbol)keyValue.value;
                    if (!label.statement && (!label.deleted || label.iasm))
                    {
                        .error(label.loc, "%s `%s` label `%s` is undefined", funcdecl.kind, funcdecl.toPrettyChars, label.toChars());
                    }
                }

            // Fix up forward-referenced gotos
            if (funcdecl.gotos && !funcdecl.isCsymbol())
            {
                for (size_t i = 0; i < funcdecl.gotos.length; ++i)
                {
                    (*funcdecl.gotos)[i].checkLabel();
                }
            }

            if (funcdecl.isNaked() && (funcdecl.fensures || funcdecl.frequires))
                .error(funcdecl.loc, "%s `%s` naked assembly functions with contracts are not supported", funcdecl.kind, funcdecl.toPrettyChars);

            sc2.ctorflow.callSuper = CSX.none;
            sc2.pop();
        }

        if (funcdecl.checkClosure())
        {
            // We should be setting errors here instead of relying on the global error count.
            //errors = true;
        }

        /* If function survived being marked as impure, then it is pure
         */
        if (funcdecl.purityInprocess)
        {
            funcdecl.purityInprocess = false;
            if (funcdecl.type == f)
                f = cast(TypeFunction)f.copy();
            f.purity = PURE.fwdref;
        }

        if (funcdecl.safetyInprocess)
        {
            funcdecl.safetyInprocess = false;
            if (funcdecl.type == f)
                f = cast(TypeFunction)f.copy();
            f.trust = TRUST.safe;
        }

        if (funcdecl.nogcInprocess)
        {
            funcdecl.nogcInprocess = false;
            if (funcdecl.type == f)
                f = cast(TypeFunction)f.copy();
            f.isNogc = true;
        }

        finishScopeParamInference(funcdecl, f);

        // reset deco to apply inference result to mangled name
        if (f != funcdecl.type)
            f.deco = null;

        // Do semantic type AFTER pure/nothrow inference.
        if (!f.deco && funcdecl.ident != Id.xopEquals && funcdecl.ident != Id.xopCmp)
        {
            sc = sc.push();
            if (funcdecl.isCtorDeclaration()) // https://issues.dlang.org/show_bug.cgi?id=#15665
                f.isCtor = true;
            sc.stc = STC.none;
            sc.linkage = funcdecl._linkage; // https://issues.dlang.org/show_bug.cgi?id=8496
            funcdecl.type = f.typeSemantic(funcdecl.loc, sc);
            sc = sc.pop();
        }

        // Check `extern(C++)` functions for invalid the return/parameter types
        if (funcdecl._linkage == LINK.cpp)
        {
            static bool isCppNonMappableType(Type type, Parameter param = null, Type origType = null)
            {
                // Don't allow D `immutable` and `shared` types to be interfaced with C++
                if (type.isImmutable() || type.isShared())
                    return true;
                if (Type cpptype = target.cpp.parameterType(type))
                    type = cpptype;

                if (origType is null)
                    origType = type;

                // Permit types that are handled by toCppMangle. This list should be kept in sync with
                // each visit method in dmd.cppmangle and dmd.cppmanglewin.
                switch (type.ty)
                {
                    case Tnull:
                    case Tnoreturn:
                    case Tvector:
                    case Tpointer:
                    case Treference:
                    case Tfunction:
                    case Tstruct:
                    case Tenum:
                    case Tclass:
                    case Tident:
                    case Tinstance:
                        break;

                    case Tsarray:
                        if (!origType.isTypePointer())
                            return true;
                        break;

                    default:
                        if (!type.isTypeBasic())
                            return true;
                        break;
                }

                // Descend to the enclosing type
                if (auto tnext = type.nextOf())
                    return isCppNonMappableType(tnext, param, origType);

                return false;
            }
            if (isCppNonMappableType(f.next.toBasetype()))
            {
                .error(funcdecl.loc, "%s `%s` cannot return type `%s` because its linkage is `extern(C++)`", funcdecl.kind, funcdecl.toErrMsg(), f.next.toChars());
                if (f.next.isTypeDArray())
                    errorSupplemental(funcdecl.loc, "slices are specific to D and do not have a counterpart representation in C++", f.next.toChars());
                funcdecl.errors = true;
            }
            foreach (i, param; f.parameterList)
            {
                if (isCppNonMappableType(param.type.toBasetype(), param))
                {
                    .error(funcdecl.loc, "%s `%s` cannot have parameter of type `%s` because its linkage is `extern(C++)`", funcdecl.kind, funcdecl.toErrMsg(), param.type.toChars());
                    if (param.type.toBasetype().isTypeSArray())
                        errorSupplemental(funcdecl.loc, "perhaps use a `%s*` type instead",
                                          param.type.nextOf().mutableOf().unSharedOf().toChars());
                    funcdecl.errors = true;
                }
            }
        }

        // Do live analysis
        if (sc.previews.dip1021 && funcdecl.fbody && funcdecl.type.ty != Terror &&
            funcdecl.type.isTypeFunction().isLive)
        {
            oblive(funcdecl);
        }

        /* If this function had instantiated with gagging, error reproduction will be
         * done by TemplateInstance::semantic.
         * Otherwise, error gagging should be temporarily ungagged by functionSemantic3.
         */
        funcdecl.semanticRun = PASS.semantic3done;
        if ((global.errors != oldErrors) || (funcdecl.fbody && funcdecl.fbody.isErrorStatement()))
            funcdecl.hasSemantic3Errors = true;
        else
            funcdecl.hasSemantic3Errors = false;
        if (funcdecl.type.ty == Terror)
            funcdecl.errors = true;
        //printf("-FuncDeclaration::semantic3('%s.%s', sc = %p, loc = %s)\n", funcdecl.parent.toChars(), funcdecl.toChars(), sc, funcdecl.loc.toChars());
        //fflush(stdout);
    }

    override void visit(CtorDeclaration ctor)
    {
        //printf("CtorDeclaration()\n%s\n", ctor.fbody.toChars());
        if (ctor.semanticRun >= PASS.semantic3)
            return;

        /* If any of the fields of the aggregate have a destructor, add
         *   scope (failure) { this.fieldDtor(); }
         * as the first statement of the constructor (unless the constructor
         * doesn't define a body - @disable, extern)
         *.It is not necessary to add it after
         * each initialization of a field, because destruction of .init constructed
         * structs should be benign.
         * https://issues.dlang.org/show_bug.cgi?id=14246
         */
        AggregateDeclaration ad = ctor.isMemberDecl();
        if (!ctor.fbody || !ad || !ad.fieldDtor ||
            global.params.dtorFields == FeatureState.disabled || !global.params.useExceptions || ctor.type.toTypeFunction.isNothrow)
            return visit(cast(FuncDeclaration)ctor);

        /* Generate:
         *   this.fieldDtor()
         */
        Expression e = new ThisExp(ctor.loc);
        e.type = ad.type.mutableOf();
        e = new DotVarExp(ctor.loc, e, ad.fieldDtor, false);
        auto ce = new CallExp(ctor.loc, e);
        auto sexp = new ExpStatement(ctor.loc, ce);
        auto ss = new ScopeStatement(ctor.loc, sexp, ctor.loc);

        if (global.params.dtorFields == FeatureState.default_)
        {
            auto ctf = cast(TypeFunction) ctor.type;
            auto dtf = cast(TypeFunction) ad.fieldDtor.type;

            const ngErr = ctf.isNogc && !dtf.isNogc;
            const puErr = ctf.purity && !dtf.purity;
            const saErr = ctf.trust == TRUST.safe && dtf.trust <= TRUST.system;

            if (ngErr || puErr || saErr)
            {
                // storage_class is apparently not set for dtor & ctor
                OutBuffer ob;
                stcToBuffer(ob,
                    (ngErr ? STC.nogc : STC.none) |
                    (puErr ? STC.pure_ : STC.none) |
                    (saErr ? STC.system : STC.none)
                );
                ctor.loc.error("`%s` has stricter attributes than its destructor (`%s`)", ctor.toPrettyChars(), ob.peekChars());
                ctor.loc.errorSupplemental("The destructor will be called if an exception is thrown");
                ctor.loc.errorSupplemental("Either make the constructor `nothrow` or adjust the field destructors");

                ce.ignoreAttributes = true;
            }
        }

        version (all)
        {
            /* Generate:
             *   try { ctor.fbody; }
             *   catch (Exception __o)
             *   { this.fieldDtor(); throw __o; }
             * This differs from the alternate scope(failure) version in that an Exception
             * is caught rather than a Throwable. This enables the optimization whereby
             * the try-catch can be removed if ctor.fbody is nothrow. (nothrow only
             * applies to Exception.)
             */
            Identifier id = Identifier.generateId("__o");
            auto ts = new ThrowStatement(ctor.loc, new IdentifierExp(ctor.loc, id));
            auto handler = new CompoundStatement(ctor.loc, ss, ts);

            auto catches = new Catches();
            auto ctch = new Catch(ctor.loc, getException(), id, handler);
            catches.push(ctch);

            ctor.fbody = new TryCatchStatement(ctor.loc, ctor.fbody, catches);
        }
        else
        {
            /* Generate:
             *   scope (failure) { this.fieldDtor(); }
             * Hopefully we can use this version someday when scope(failure) catches
             * Exception instead of Throwable.
             */
            auto s = new ScopeGuardStatement(ctor.loc, TOK.onScopeFailure, ss);
            ctor.fbody = new CompoundStatement(ctor.loc, s, ctor.fbody);
        }
        visit(cast(FuncDeclaration)ctor);
    }


    override void visit(Nspace ns)
    {
        if (ns.semanticRun >= PASS.semantic3)
            return;
        ns.semanticRun = PASS.semantic3;
        static if (LOG)
        {
            printf("Nspace::semantic3('%s')\n", ns.toChars());
        }
        if (!ns.members)
            return;

        sc = sc.push(ns);
        sc.linkage = LINK.cpp;
        foreach (s; *ns.members)
        {
            s.semantic3(sc);
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
            s.semantic3(sc2);
        }
        if (sc2 != sc)
            sc2.pop();
    }

    override void visit(AggregateDeclaration ad)
    {
        //printf("AggregateDeclaration::semantic3(sc=%p, %s) type = %s, errors = %d\n", sc, toChars(), type.toChars(), errors);
        if (!ad.members)
            return;

        StructDeclaration sd = ad.isStructDeclaration();
        if (!sc) // from runDeferredSemantic3 for TypeInfo generation
        {
            assert(sd);
            sd.semanticTypeInfoMembers();
            return;
        }

        auto sc2 = ad.newScope(sc);

        for (size_t i = 0; i < ad.members.length; i++)
        {
            Dsymbol s = (*ad.members)[i];
            s.semantic3(sc2);
        }

        sc2.pop();

        // Instantiate RTInfo!S to provide a pointer bitmap for the GC
        // Don't do it in -betterC or on unused deprecated / error types
        if (!ad.getRTInfo && global.params.useTypeInfo && Type.rtinfo &&
            (!ad.isDeprecated() || global.params.useDeprecated != DiagnosticReporting.error) &&
            (ad.type && ad.type.ty != Terror))
        {
            // Evaluate: RTinfo!type
            auto tiargs = new Objects();
            tiargs.push(ad.type);
            auto ti = new TemplateInstance(ad.loc, Type.rtinfo, tiargs);

            Scope* sc3 = ti.tempdecl._scope.startCTFE();
            sc3.tinst = sc.tinst;
            sc3.minst = sc.minst;
            if (ad.isDeprecated())
                sc3.stc |= STC.deprecated_;

            ti.dsymbolSemantic(sc3);
            ti.semantic2(sc3);
            ti.semantic3(sc3);
            auto e = symbolToExp(ti.toAlias(), Loc.initial, sc3, false);

            sc3.endCTFE();

            e = e.ctfeInterpret();
            ad.getRTInfo = e;
        }
        if (sd)
            sd.semanticTypeInfoMembers();
        ad.semanticRun = PASS.semantic3done;
    }
}

private struct FuncDeclSem3
{
    // The FuncDeclaration subject to Semantic analysis
    FuncDeclaration funcdecl;

    // Scope of analysis
    Scope* sc;
    this(FuncDeclaration fd,Scope* s) scope @safe
    {
        funcdecl = fd;
        sc = s;
    }

    /* Checks that the overridden functions (if any) have in contracts if
     * funcdecl has an in contract.
     */
    void checkInContractOverrides()
    {
        if (funcdecl.frequires)
        {
            for (size_t i = 0; i < funcdecl.foverrides.length; i++)
            {
                FuncDeclaration fdv = funcdecl.foverrides[i];
                if (fdv.fbody && !fdv.frequires)
                {
                    .error(funcdecl.loc, "%s `%s` cannot have an in contract when overridden function `%s` does not have an in contract", funcdecl.kind, funcdecl.toPrettyChars, fdv.toPrettyChars());
                    break;
                }
            }
        }
    }
}

void semanticTypeInfoMembers(StructDeclaration sd)
{
    if (sd.xeq &&
        sd.xeq._scope &&
        sd.xeq.semanticRun < PASS.semantic3done)
    {
        const errors = global.startGagging();
        sd.xeq.semantic3(sd.xeq._scope);
        if (global.endGagging(errors))
            sd.xeq = sd.xerreq;
    }

    if (sd.xcmp &&
        sd.xcmp._scope &&
        sd.xcmp.semanticRun < PASS.semantic3done)
    {
        const errors = global.startGagging();
        sd.xcmp.semantic3(sd.xcmp._scope);
        if (global.endGagging(errors))
            sd.xcmp = sd.xerrcmp;
    }

    FuncDeclaration ftostr = search_toString(sd);
    if (ftostr &&
        ftostr._scope &&
        ftostr.semanticRun < PASS.semantic3done)
    {
        ftostr.semantic3(ftostr._scope);
    }

    if (sd.xhash &&
        sd.xhash._scope &&
        sd.xhash.semanticRun < PASS.semantic3done)
    {
        sd.xhash.semantic3(sd.xhash._scope);
    }

    if (sd.postblit &&
        sd.postblit._scope &&
        sd.postblit.semanticRun < PASS.semantic3done)
    {
        sd.postblit.semantic3(sd.postblit._scope);
    }

    if (sd.dtor &&
        sd.dtor._scope &&
        sd.dtor.semanticRun < PASS.semantic3done)
    {
        sd.dtor.semantic3(sd.dtor._scope);
    }
}

/***********************************************
 * Check that the function contains any closure.
 * If it's @nogc, report suitable errors.
 * This is mostly consistent with FuncDeclaration::needsClosure().
 *
 * Returns:
 *      true if any errors occur.
 */
extern (D) bool checkClosure(FuncDeclaration fd)
{
    //printf("checkClosure() %s\n", toPrettyChars());
    if (!fd.needsClosure())
        return false;

    if (fd.setGC(fd.loc, "allocating a closure for `%s()`", fd))
    {
        .error(fd.loc, "%s `%s` is `@nogc` yet allocates closure for `%s()` with the GC", fd.kind, fd.toPrettyChars(), fd.toChars());
        if (global.gag)     // need not report supplemental errors
            return true;
    }
    else if (!global.params.useGC)
    {
        .error(fd.loc, "%s `%s` is `-betterC` yet allocates closure for `%s()` with the GC", fd.kind, fd.toPrettyChars(), fd.toChars());
        if (global.gag)     // need not report supplemental errors
            return true;
    }
    else
    {
        fd.printGCUsage(fd.loc, "using closure causes GC allocation");
        return false;
    }

    FuncDeclarations a;
    foreach (v; fd.closureVars)
    {
        foreach (f; v.nestedrefs)
        {
            assert(f !is fd);

        LcheckAncestorsOfANestedRef:
            for (Dsymbol s = f; s && s !is fd; s = s.toParentP(fd))
            {
                auto fx = s.isFuncDeclaration();
                if (!fx)
                    continue;
                if (fx.isThis() ||
                    fx.tookAddressOf ||
                    checkEscapingSiblings(fx, fd))
                {
                    foreach (f2; a)
                    {
                        if (f2 == f)
                            break LcheckAncestorsOfANestedRef;
                    }
                    a.push(f);
                    .errorSupplemental(f.loc, "%s `%s` closes over variable `%s`",
                        f.kind, f.toErrMsg(), v.toChars());
                    if (v.ident != Id.This)
                        .errorSupplemental(v.loc, "`%s` declared here", v.toChars());

                    break LcheckAncestorsOfANestedRef;
                }
            }
        }
    }

    return true;
}
