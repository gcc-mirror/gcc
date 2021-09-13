
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "dsymbol.h"
#include "aggregate.h"
#include "attrib.h"
#include "declaration.h"
#include "errors.h"
#include "id.h"
#include "init.h"
#include "module.h"
#include "nspace.h"
#include "scope.h"
#include "statement.h"
#include "statement_rewrite_walker.h"
#include "target.h"
#include "template.h"
#include "visitor.h"

bool allowsContractWithoutBody(FuncDeclaration *funcdecl);
int blockExit(Statement *s, FuncDeclaration *func, bool mustNotThrow);
bool checkReturnEscape(Scope *sc, Expression *e, bool gag);
bool checkReturnEscapeRef(Scope *sc, Expression *e, bool gag);
TypeIdentifier *getThrowable();
char *MODtoChars(MOD mod);
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads);
void allocFieldinit(Scope *sc, size_t dim);
void freeFieldinit(Scope *sc);

/* Determine if function should add `return 0;`
 */
static bool addReturn0(FuncDeclaration *funcdecl)
{
    TypeFunction *f = (TypeFunction *)funcdecl->type;

    return f->next->ty == Tvoid &&
        (funcdecl->isMain() || (global.params.betterC && funcdecl->isCMain()));
}

/********************************************************
 * Generate Expression to call the invariant.
 * Input:
 *      ad      aggregate with the invariant
 *      vthis   variable with 'this'
 * Returns:
 *      void expression that calls the invariant
 */
static Expression *addInvariant(AggregateDeclaration *ad, VarDeclaration *vthis)
{
    Expression *e = NULL;

    // Call invariant directly only if it exists
    FuncDeclaration *inv = ad->inv;
    ClassDeclaration *cd = ad->isClassDeclaration();

    while (!inv && cd)
    {
        cd = cd->baseClass;
        if (!cd)
            break;
        inv = cd->inv;
    }
    if (inv)
    {
    #if 1
        // Workaround for bugzilla 13394: For the correct mangling,
        // run attribute inference on inv if needed.
        inv->functionSemantic();
    #endif

        //e = new DsymbolExp(Loc(), inv);
        //e = new CallExp(Loc(), e);
        //dsymbolSemantic(e, sc2);

        /* https://issues.dlang.org/show_bug.cgi?id=13113
         * Currently virtual invariant calls completely
         * bypass attribute enforcement.
         * Change the behavior of pre-invariant call by following it.
         */
        e = new ThisExp(Loc());
        e->type = vthis->type;
        e = new DotVarExp(Loc(), e, inv, false);
        e->type = inv->type;
        e = new CallExp(Loc(), e);
        e->type = Type::tvoid;
    }
    return e;
}

/* Tweak all return statements and dtor call for nrvo_var, for correct NRVO.
 */
class NrvoWalker : public StatementRewriteWalker
{
public:
    FuncDeclaration *fd;
    Scope *sc;

    void visit(ReturnStatement *s)
    {
        // See if all returns are instead to be replaced with a goto returnLabel;
        if (fd->returnLabel)
        {
            /* Rewrite:
             *  return exp;
             * as:
             *  vresult = exp; goto Lresult;
             */
            GotoStatement *gs = new GotoStatement(s->loc, Id::returnLabel);
            gs->label = fd->returnLabel;

            Statement *s1 = gs;
            if (s->exp)
                s1 = new CompoundStatement(s->loc, new ExpStatement(s->loc, s->exp), gs);

            replaceCurrent(s1);
        }
    }
    void visit(TryFinallyStatement *s)
    {
        DtorExpStatement *des;
        if (fd->nrvo_can &&
            s->finalbody && (des = s->finalbody->isDtorExpStatement()) != NULL &&
            fd->nrvo_var == des->var)
        {
            if (!(global.params.useExceptions && ClassDeclaration::throwable))
            {
                /* Don't need to call destructor at all, since it is nrvo
                 */
                replaceCurrent(s->_body);
                s->_body->accept(this);
                return;
            }

            /* Normally local variable dtors are called regardless exceptions.
             * But for nrvo_var, its dtor should be called only when exception is thrown.
             *
             * Rewrite:
             *      try { s->body; } finally { nrvo_var->edtor; }
             *      // equivalent with:
             *      //    s->body; scope(exit) nrvo_var->edtor;
             * as:
             *      try { s->body; } catch(Throwable __o) { nrvo_var->edtor; throw __o; }
             *      // equivalent with:
             *      //    s->body; scope(failure) nrvo_var->edtor;
             */
            Statement *sexception = new DtorExpStatement(Loc(), fd->nrvo_var->edtor, fd->nrvo_var);
            Identifier *id = Identifier::generateId("__o");

            Statement *handler = new PeelStatement(sexception);
            if (blockExit(sexception, fd, false) & BEfallthru)
            {
                ThrowStatement *ts = new ThrowStatement(Loc(), new IdentifierExp(Loc(), id));
                ts->internalThrow = true;
                handler = new CompoundStatement(Loc(), handler, ts);
            }

            Catches *catches = new Catches();
            Catch *ctch = new Catch(Loc(), getThrowable(), id, handler);
            ctch->internalCatch = true;
            catchSemantic(ctch, sc);     // Run semantic to resolve identifier '__o'
            catches->push(ctch);

            Statement *s2 = new TryCatchStatement(Loc(), s->_body, catches);
            replaceCurrent(s2);
            s2->accept(this);
        }
        else
            StatementRewriteWalker::visit(s);
    }
};

class Semantic3Visitor : public Visitor
{
public:
    Scope *sc;

    Semantic3Visitor(Scope *sc)
    {
        this->sc = sc;
    }

    void visit(Dsymbol *)
    {
        // Most Dsymbols have no further semantic analysis needed
    }

    void visit(TemplateInstance *tempinst)
    {
        //if (tempinst->toChars()[0] == 'D') *(char*)0=0;
        if (tempinst->semanticRun >= PASSsemantic3)
            return;
        tempinst->semanticRun = PASSsemantic3;
        if (!tempinst->errors && tempinst->members)
        {
            TemplateDeclaration *tempdecl = tempinst->tempdecl->isTemplateDeclaration();
            assert(tempdecl);

            sc = tempdecl->_scope;
            sc = sc->push(tempinst->argsym);
            sc = sc->push(tempinst);
            sc->tinst = tempinst;
            sc->minst = tempinst->minst;

            int needGagging = (tempinst->gagged && !global.gag);
            unsigned int olderrors = global.errors;
            int oldGaggedErrors = -1;       // dead-store to prevent spurious warning
            /* If this is a gagged instantiation, gag errors.
             * Future optimisation: If the results are actually needed, errors
             * would already be gagged, so we don't really need to run semantic
             * on the members.
             */
            if (needGagging)
                oldGaggedErrors = global.startGagging();

            for (size_t i = 0; i < tempinst->members->length; i++)
            {
                Dsymbol *s = (*tempinst->members)[i];
                semantic3(s, sc);
                if (tempinst->gagged && global.errors != olderrors)
                    break;
            }

            if (global.errors != olderrors)
            {
                if (!tempinst->errors)
                {
                    if (!tempdecl->literal)
                        tempinst->error(tempinst->loc, "error instantiating");
                    if (tempinst->tinst)
                        tempinst->tinst->printInstantiationTrace();
                }
                tempinst->errors = true;
            }
            if (needGagging)
                global.endGagging(oldGaggedErrors);

            sc = sc->pop();
            sc->pop();
        }
    }

    void visit(TemplateMixin *tmix)
    {
        if (tmix->semanticRun >= PASSsemantic3)
            return;
        tmix->semanticRun = PASSsemantic3;
        if (tmix->members)
        {
            sc = sc->push(tmix->argsym);
            sc = sc->push(tmix);
            for (size_t i = 0; i < tmix->members->length; i++)
            {
                Dsymbol *s = (*tmix->members)[i];
                semantic3(s, sc);
            }
            sc = sc->pop();
            sc->pop();
        }
    }

    void visit(Module *mod)
    {
        //printf("Module::semantic3('%s'): parent = %p\n", mod->toChars(), mod->parent);
        if (mod->semanticRun != PASSsemantic2done)
            return;
        mod->semanticRun = PASSsemantic3;

        // Note that modules get their own scope, from scratch.
        // This is so regardless of where in the syntax a module
        // gets imported, it is unaffected by context.
        Scope *sc = Scope::createGlobal(mod);      // create root scope
        //printf("Module = %p\n", sc.scopesym);

        // Pass 3 semantic routines: do initializers and function bodies
        for (size_t i = 0; i < mod->members->length; i++)
        {
            Dsymbol *s = (*mod->members)[i];
            //printf("Module %s: %s.semantic3()\n", mod->toChars(), s->toChars());
            semantic3(s, sc);

            mod->runDeferredSemantic2();
        }

        if (mod->userAttribDecl)
        {
            semantic3(mod->userAttribDecl, sc);
        }

        sc = sc->pop();
        sc->pop();
        mod->semanticRun = PASSsemantic3done;
    }

    void visit(FuncDeclaration *funcdecl)
    {
        VarDeclaration *_arguments = NULL;

        if (!funcdecl->parent)
        {
            if (global.errors)
                return;
            //printf("FuncDeclaration::semantic3(%s '%s', sc = %p)\n", funcdecl->kind(), funcdecl->toChars(), sc);
            assert(0);
        }
        if (funcdecl->errors || isError(funcdecl->parent))
        {
            funcdecl->errors = true;
            return;
        }
        //printf("FuncDeclaration::semantic3('%s.%s', %p, sc = %p, loc = %s)\n", funcdecl->parent->toChars(), funcdecl->toChars(), funcdecl, sc, funcdecl->loc.toChars());
        //fflush(stdout);
        //printf("storage class = x%x %x\n", sc->stc, funcdecl->storage_class);
        //{ static int x; if (++x == 2) *(char*)0=0; }
        //printf("\tlinkage = %d\n", sc->linkage);

        if (funcdecl->ident == Id::assign && !funcdecl->inuse)
        {
            if (funcdecl->storage_class & STCinference)
            {
                /* Bugzilla 15044: For generated opAssign function, any errors
                 * from its body need to be gagged.
                 */
                unsigned oldErrors = global.startGagging();
                funcdecl->inuse++;
                semantic3(funcdecl, sc);
                funcdecl->inuse--;
                if (global.endGagging(oldErrors))   // if errors happened
                {
                    // Disable generated opAssign, because some members forbid identity assignment.
                    funcdecl->storage_class |= STCdisable;
                    funcdecl->fbody = NULL;   // remove fbody which contains the error
                    funcdecl->semantic3Errors = false;
                }
                return;
            }
        }

        //printf(" sc->incontract = %d\n", (sc->flags & SCOPEcontract));
        if (funcdecl->semanticRun >= PASSsemantic3)
            return;
        funcdecl->semanticRun = PASSsemantic3;
        funcdecl->semantic3Errors = false;

        if (!funcdecl->type || funcdecl->type->ty != Tfunction)
            return;
        TypeFunction *f = (TypeFunction *)funcdecl->type;
        if (!funcdecl->inferRetType && f->next->ty == Terror)
            return;

        if (!funcdecl->fbody && funcdecl->inferRetType && !f->next)
        {
            funcdecl->error("has no function body with return type inference");
            return;
        }

        unsigned oldErrors = global.errors;

        if (funcdecl->frequires)
        {
            for (size_t i = 0; i < funcdecl->foverrides.length; i++)
            {
                FuncDeclaration *fdv = funcdecl->foverrides[i];

                if (fdv->fbody && !fdv->frequires)
                {
                    funcdecl->error("cannot have an in contract when overriden function %s does not have an in contract", fdv->toPrettyChars());
                    break;
                }
            }
        }

        // Remember whether we need to generate an 'out' contract.
        const bool needEnsure = FuncDeclaration::needsFensure(funcdecl);

        if (funcdecl->fbody || funcdecl->frequires || needEnsure)
        {
            /* Symbol table into which we place parameters and nested functions,
             * solely to diagnose name collisions.
             */
            funcdecl->localsymtab = new DsymbolTable();

            // Establish function scope
            ScopeDsymbol *ss = new ScopeDsymbol();
            // find enclosing scope symbol, might skip symbol-less CTFE and/or FuncExp scopes
            for (Scope *scx = sc; ; scx = scx->enclosing)
            {
                if (scx->scopesym)
                {
                    ss->parent = scx->scopesym;
                    break;
                }
            }
            ss->loc = funcdecl->loc;
            ss->endlinnum = funcdecl->endloc.linnum;
            Scope *sc2 = sc->push(ss);
            sc2->func = funcdecl;
            sc2->parent = funcdecl;
            sc2->callSuper = 0;
            sc2->sbreak = NULL;
            sc2->scontinue = NULL;
            sc2->sw = NULL;
            sc2->fes = funcdecl->fes;
            sc2->linkage = LINKd;
            sc2->stc &= ~(STCauto | STCscope | STCstatic | STCextern | STCabstract |
                          STCdeprecated | STCoverride |
                          STC_TYPECTOR | STCfinal | STCtls | STCgshared | STCref | STCreturn |
                          STCproperty | STCnothrow | STCpure | STCsafe | STCtrusted | STCsystem);
            sc2->protection = Prot(Prot::public_);
            sc2->explicitProtection = 0;
            sc2->aligndecl = NULL;
            if (funcdecl->ident != Id::require && funcdecl->ident != Id::ensure)
                sc2->flags = sc->flags & ~SCOPEcontract;
            sc2->flags &= ~SCOPEcompile;
            sc2->tf = NULL;
            sc2->os = NULL;
            sc2->noctor = 0;
            sc2->userAttribDecl = NULL;
            if (sc2->intypeof == 1) sc2->intypeof = 2;
            sc2->fieldinit = NULL;
            sc2->fieldinit_dim = 0;

            /* Note: When a lambda is defined immediately under aggregate member
             * scope, it should be contextless due to prevent interior pointers.
             * e.g.
             *      // dg points 'this' - it's interior pointer
             *      class C { int x; void delegate() dg = (){ this.x = 1; }; }
             *
             * However, lambdas could be used inside typeof, in order to check
             * some expressions varidity at compile time. For such case the lambda
             * body can access aggregate instance members.
             * e.g.
             *      class C { int x; static assert(is(typeof({ this.x = 1; }))); }
             *
             * To properly accept it, mark these lambdas as member functions.
             */
            if (FuncLiteralDeclaration *fld = funcdecl->isFuncLiteralDeclaration())
            {
                if (AggregateDeclaration *ad = funcdecl->isMember2())
                {
                    if (!sc->intypeof)
                    {
                        if (fld->tok == TOKdelegate)
                            funcdecl->error("cannot be %s members", ad->kind());
                        else
                            fld->tok = TOKfunction;
                    }
                    else
                    {
                        if (fld->tok != TOKfunction)
                            fld->tok = TOKdelegate;
                    }
                }
            }

            // Declare 'this'
            AggregateDeclaration *ad = funcdecl->isThis();
            funcdecl->vthis = funcdecl->declareThis(sc2, ad);
            //printf("[%s] ad = %p vthis = %p\n", funcdecl->loc.toChars(), ad, funcdecl->vthis);
            //if (funcdecl->vthis) printf("\tvthis->type = %s\n", funcdecl->vthis->type->toChars());

            // Declare hidden variable _arguments[] and _argptr
            if (f->parameterList.varargs == VARARGvariadic)
            {
                if (f->linkage == LINKd)
                {
                    // Variadic arguments depend on Typeinfo being defined
                    if (!global.params.useTypeInfo || !Type::dtypeinfo || !Type::typeinfotypelist)
                    {
                        if (!global.params.useTypeInfo)
                            funcdecl->error("D-style variadic functions cannot be used with -betterC");
                        else if (!Type::typeinfotypelist)
                            funcdecl->error("`object.TypeInfo_Tuple` could not be found, but is implicitly used in D-style variadic functions");
                        else
                            funcdecl->error("`object.TypeInfo` could not be found, but is implicitly used in D-style variadic functions");
                        fatal();
                    }

                    // Declare _arguments[]
                    funcdecl->v_arguments = new VarDeclaration(Loc(), Type::typeinfotypelist->type, Id::_arguments_typeinfo, NULL);
                    funcdecl->v_arguments->storage_class |= STCtemp | STCparameter;
                    dsymbolSemantic(funcdecl->v_arguments, sc2);
                    sc2->insert(funcdecl->v_arguments);
                    funcdecl->v_arguments->parent = funcdecl;

                    //Type *t = Type::typeinfo->type->constOf()->arrayOf();
                    Type *t = Type::dtypeinfo->type->arrayOf();
                    _arguments = new VarDeclaration(Loc(), t, Id::_arguments, NULL);
                    _arguments->storage_class |= STCtemp;
                    dsymbolSemantic(_arguments, sc2);
                    sc2->insert(_arguments);
                    _arguments->parent = funcdecl;
                }
                if (f->linkage == LINKd || f->parameterList.length())
                {
                    // Declare _argptr
                    Type *t = target.va_listType(funcdecl->loc, sc);
                    funcdecl->v_argptr = new VarDeclaration(Loc(), t, Id::_argptr, NULL);
                    funcdecl->v_argptr->storage_class |= STCtemp;
                    dsymbolSemantic(funcdecl->v_argptr, sc2);
                    sc2->insert(funcdecl->v_argptr);
                    funcdecl->v_argptr->parent = funcdecl;
                }
            }

            /* Declare all the function parameters as variables
             * and install them in parameters[]
             */
            size_t nparams = f->parameterList.length();
            if (nparams)
            {
                /* parameters[] has all the tuples removed, as the back end
                 * doesn't know about tuples
                 */
                funcdecl->parameters = new VarDeclarations();
                funcdecl->parameters->reserve(nparams);
                for (size_t i = 0; i < nparams; i++)
                {
                    Parameter *fparam = f->parameterList[i];
                    Identifier *id = fparam->ident;
                    StorageClass stc = 0;
                    if (!id)
                    {
                        /* Generate identifier for un-named parameter,
                         * because we need it later on.
                         */
                        fparam->ident = id = Identifier::generateId("_param_", i);
                        stc |= STCtemp;
                    }
                    Type *vtype = fparam->type;
                    VarDeclaration *v = new VarDeclaration(funcdecl->loc, vtype, id, NULL);
                    //printf("declaring parameter %s of type %s\n", v->toChars(), v->type->toChars());
                    stc |= STCparameter;
                    if (f->parameterList.varargs == VARARGtypesafe && i + 1 == nparams)
                        stc |= STCvariadic;
                    if (funcdecl->flags & FUNCFLAGinferScope && !(fparam->storageClass & STCscope))
                        stc |= STCmaybescope;
                    stc |= fparam->storageClass & (STCin | STCout | STCref | STCreturn | STCscope | STClazy | STCfinal | STC_TYPECTOR | STCnodtor);
                    v->storage_class = stc;
                    dsymbolSemantic(v, sc2);
                    if (!sc2->insert(v))
                        funcdecl->error("parameter %s.%s is already defined", funcdecl->toChars(), v->toChars());
                    else
                        funcdecl->parameters->push(v);
                    funcdecl->localsymtab->insert(v);
                    v->parent = funcdecl;
                    if (fparam->userAttribDecl)
                        v->userAttribDecl = fparam->userAttribDecl;
                }
            }

            // Declare the tuple symbols and put them in the symbol table,
            // but not in parameters[].
            if (f->parameterList.parameters)
            {
                for (size_t i = 0; i < f->parameterList.parameters->length; i++)
                {
                    Parameter *fparam = (*f->parameterList.parameters)[i];

                    if (!fparam->ident)
                        continue;                   // never used, so ignore
                    if (fparam->type->ty == Ttuple)
                    {
                        TypeTuple *t = (TypeTuple *)fparam->type;
                        size_t dim = Parameter::dim(t->arguments);
                        Objects *exps = new Objects();
                        exps->setDim(dim);
                        for (size_t j = 0; j < dim; j++)
                        {
                            Parameter *narg = Parameter::getNth(t->arguments, j);
                            assert(narg->ident);
                            VarDeclaration *v = sc2->search(Loc(), narg->ident, NULL)->isVarDeclaration();
                            assert(v);
                            Expression *e = new VarExp(v->loc, v);
                            (*exps)[j] = e;
                        }
                        assert(fparam->ident);
                        TupleDeclaration *v = new TupleDeclaration(funcdecl->loc, fparam->ident, exps);
                        //printf("declaring tuple %s\n", v->toChars());
                        v->isexp = true;
                        if (!sc2->insert(v))
                            funcdecl->error("parameter %s.%s is already defined", funcdecl->toChars(), v->toChars());
                        funcdecl->localsymtab->insert(v);
                        v->parent = funcdecl;
                    }
                }
            }

            // Precondition invariant
            Statement *fpreinv = NULL;
            if (funcdecl->addPreInvariant())
            {
                Expression *e = addInvariant(ad, funcdecl->vthis);
                if (e)
                    fpreinv = new ExpStatement(Loc(), e);
            }

            // Postcondition invariant
            Statement *fpostinv = NULL;
            if (funcdecl->addPostInvariant())
            {
                Expression *e = addInvariant(ad, funcdecl->vthis);
                if (e)
                    fpostinv = new ExpStatement(Loc(), e);
            }

            // Pre/Postcondition contract
            if (!funcdecl->fbody)
                funcdecl->buildEnsureRequire();

            Scope *scout = NULL;
            if (needEnsure || funcdecl->addPostInvariant())
            {
                if ((needEnsure && global.params.useOut == CHECKENABLEon) || fpostinv)
                {
                    funcdecl->returnLabel = new LabelDsymbol(Id::returnLabel);
                }

                // scope of out contract (need for vresult->semantic)
                ScopeDsymbol *sym = new ScopeDsymbol();
                sym->parent = sc2->scopesym;
                sym->loc = funcdecl->loc;
                sym->endlinnum = funcdecl->endloc.linnum;
                scout = sc2->push(sym);
            }

            if (funcdecl->fbody)
            {
                ScopeDsymbol *sym = new ScopeDsymbol();
                sym->parent = sc2->scopesym;
                sym->loc = funcdecl->loc;
                sym->endlinnum = funcdecl->endloc.linnum;
                sc2 = sc2->push(sym);

                AggregateDeclaration *ad2 = funcdecl->isMember2();

                /* If this is a class constructor
                */
                if (ad2 && funcdecl->isCtorDeclaration())
                {
                    allocFieldinit(sc2, ad2->fields.length);
                    for (size_t i = 0; i < ad2->fields.length; i++)
                    {
                        VarDeclaration *v = ad2->fields[i];
                        v->ctorinit = 0;
                    }
                }

                bool inferRef = (f->isref && (funcdecl->storage_class & STCauto));

                funcdecl->fbody = statementSemantic(funcdecl->fbody, sc2);
                if (!funcdecl->fbody)
                    funcdecl->fbody = new CompoundStatement(Loc(), new Statements());

                if (funcdecl->naked)
                {
                    fpreinv = NULL;         // can't accommodate with no stack frame
                    fpostinv = NULL;
                }

                assert(funcdecl->type == f ||
                       (funcdecl->type->ty == Tfunction &&
                        f->purity == PUREimpure &&
                        ((TypeFunction *)funcdecl->type)->purity >= PUREfwdref));
                f = (TypeFunction *)funcdecl->type;

                if (funcdecl->inferRetType)
                {
                    // If no return type inferred yet, then infer a void
                    if (!f->next)
                        f->next = Type::tvoid;
                    if (f->checkRetType(funcdecl->loc))
                        funcdecl->fbody = new ErrorStatement();
                }
                if (global.params.vcomplex && f->next != NULL)
                    f->next->checkComplexTransition(funcdecl->loc);

                if (funcdecl->returns && !funcdecl->fbody->isErrorStatement())
                {
                    for (size_t i = 0; i < funcdecl->returns->length; )
                    {
                        Expression *exp = (*funcdecl->returns)[i]->exp;
                        if (exp->op == TOKvar && ((VarExp *)exp)->var == funcdecl->vresult)
                        {
                            if (addReturn0(funcdecl))
                                exp->type = Type::tint32;
                            else
                                exp->type = f->next;
                            // Remove `return vresult;` from returns
                            funcdecl->returns->remove(i);
                            continue;
                        }
                        if (inferRef && f->isref && !exp->type->constConv(f->next))     // Bugzilla 13336
                            f->isref = false;
                        i++;
                    }
                }
                if (f->isref)   // Function returns a reference
                {
                    if (funcdecl->storage_class & STCauto)
                        funcdecl->storage_class &= ~STCauto;
                }
                if (!target.isReturnOnStack(f, funcdecl->needThis()) || !funcdecl->checkNRVO())
                    funcdecl->nrvo_can = 0;

                if (funcdecl->fbody->isErrorStatement())
                    ;
                else if (funcdecl->isStaticCtorDeclaration())
                {
                    /* It's a static constructor. Ensure that all
                     * ctor consts were initialized.
                     */
                    ScopeDsymbol *pd = funcdecl->toParent()->isScopeDsymbol();
                    for (size_t i = 0; i < pd->members->length; i++)
                    {
                        Dsymbol *s = (*pd->members)[i];
                        s->checkCtorConstInit();
                    }
                }
                else if (ad2 && funcdecl->isCtorDeclaration())
                {
                    ClassDeclaration *cd = ad2->isClassDeclaration();

                    // Verify that all the ctorinit fields got initialized
                    if (!(sc2->callSuper & CSXthis_ctor))
                    {
                        for (size_t i = 0; i < ad2->fields.length; i++)
                        {
                            VarDeclaration *v = ad2->fields[i];
                            if (v->isThisDeclaration())
                                continue;
                            if (v->ctorinit == 0)
                            {
                                /* Current bugs in the flow analysis:
                                 * 1. union members should not produce error messages even if
                                 *    not assigned to
                                 * 2. structs should recognize delegating opAssign calls as well
                                 *    as delegating calls to other constructors
                                 */
                                if (v->isCtorinit() && !v->type->isMutable() && cd)
                                    funcdecl->error("missing initializer for %s field %s", MODtoChars(v->type->mod), v->toChars());
                                else if (v->storage_class & STCnodefaultctor)
                                    error(funcdecl->loc, "field %s must be initialized in constructor", v->toChars());
                                else if (v->type->needsNested())
                                    error(funcdecl->loc, "field %s must be initialized in constructor, because it is nested struct", v->toChars());
                            }
                            else
                            {
                                bool mustInit = (v->storage_class & STCnodefaultctor ||
                                                 v->type->needsNested());
                                if (mustInit && !(sc2->fieldinit[i] & CSXthis_ctor))
                                {
                                    funcdecl->error("field %s must be initialized but skipped", v->toChars());
                                }
                            }
                        }
                    }
                    freeFieldinit(sc2);

                    if (cd &&
                        !(sc2->callSuper & CSXany_ctor) &&
                        cd->baseClass && cd->baseClass->ctor)
                    {
                        sc2->callSuper = 0;

                        // Insert implicit super() at start of fbody
                        FuncDeclaration *fd = resolveFuncCall(Loc(), sc2, cd->baseClass->ctor, NULL, funcdecl->vthis->type, NULL, 1);
                        if (!fd)
                        {
                            funcdecl->error("no match for implicit super() call in constructor");
                        }
                        else if (fd->storage_class & STCdisable)
                        {
                            funcdecl->error("cannot call super() implicitly because it is annotated with @disable");
                        }
                        else
                        {
                            Expression *e1 = new SuperExp(Loc());
                            Expression *e = new CallExp(Loc(), e1);
                            e = expressionSemantic(e, sc2);

                            Statement *s = new ExpStatement(Loc(), e);
                            funcdecl->fbody = new CompoundStatement(Loc(), s, funcdecl->fbody);
                        }
                    }
                    //printf("callSuper = x%x\n", sc2->callSuper);
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
                funcdecl->buildEnsureRequire();

                int blockexit = BEnone;
                if (!funcdecl->fbody->isErrorStatement())
                {
                    // Check for errors related to 'nothrow'.
                    unsigned int nothrowErrors = global.errors;
                    blockexit = blockExit(funcdecl->fbody, funcdecl, f->isnothrow);
                    if (f->isnothrow && (global.errors != nothrowErrors))
                        error(funcdecl->loc, "nothrow %s `%s` may throw", funcdecl->kind(), funcdecl->toPrettyChars());
                    if (funcdecl->flags & FUNCFLAGnothrowInprocess)
                    {
                        if (funcdecl->type == f) f = (TypeFunction *)f->copy();
                        f->isnothrow = !(blockexit & BEthrow);
                    }
                }

                if (funcdecl->fbody->isErrorStatement())
                    ;
                else if (ad2 && funcdecl->isCtorDeclaration())
                {
                    /* Append:
                     *  return this;
                     * to function body
                     */
                    if (blockexit & BEfallthru)
                    {
                        Statement *s = new ReturnStatement(funcdecl->loc, NULL);
                        s = statementSemantic(s, sc2);
                        funcdecl->fbody = new CompoundStatement(funcdecl->loc, funcdecl->fbody, s);
                        funcdecl->hasReturnExp |= (funcdecl->hasReturnExp & 1 ? 16 : 1);
                    }
                }
                else if (funcdecl->fes)
                {
                    // For foreach(){} body, append a return 0;
                    if (blockexit & BEfallthru)
                    {
                        Expression *e = new IntegerExp(0);
                        Statement *s = new ReturnStatement(Loc(), e);
                        funcdecl->fbody = new CompoundStatement(Loc(), funcdecl->fbody, s);
                        funcdecl->hasReturnExp |= (funcdecl->hasReturnExp & 1 ? 16 : 1);
                    }
                    assert(!funcdecl->returnLabel);
                }
                else if (f->next->ty == Tnoreturn)
                {
                }
                else
                {
                    const bool inlineAsm = (funcdecl->hasReturnExp & 8) != 0;
                    if ((blockexit & BEfallthru) && f->next->ty != Tvoid && !inlineAsm)
                    {
                        if (!funcdecl->hasReturnExp)
                            funcdecl->error("has no `return` statement, but is expected to return a value of type `%s`", f->next->toChars());
                        else
                            funcdecl->error("no `return exp;` or `assert(0);` at end of function");
                    }
                }

                if (funcdecl->returns)
                {
                    bool implicit0 = addReturn0(funcdecl);
                    Type *tret = implicit0 ? Type::tint32 : f->next;
                    assert(tret->ty != Tvoid);
                    if (funcdecl->vresult || funcdecl->returnLabel)
                        funcdecl->buildResultVar(scout ? scout : sc2, tret);

                    /* Cannot move this loop into NrvoWalker, because
                     * returns[i] may be in the nested delegate for foreach-body.
                     */
                    for (size_t i = 0; i < funcdecl->returns->length; i++)
                    {
                        ReturnStatement *rs = (*funcdecl->returns)[i];
                        Expression *exp = rs->exp;
                        if (exp->op == TOKerror)
                            continue;
                        if (tret->ty == Terror)
                        {
                            // Bugzilla 13702
                            exp = checkGC(sc2, exp);
                            continue;
                        }

                        if (!exp->implicitConvTo(tret) &&
                            funcdecl->parametersIntersect(exp->type))
                        {
                            if (exp->type->immutableOf()->implicitConvTo(tret))
                                exp = exp->castTo(sc2, exp->type->immutableOf());
                            else if (exp->type->wildOf()->implicitConvTo(tret))
                                exp = exp->castTo(sc2, exp->type->wildOf());
                        }
                        exp = exp->implicitCastTo(sc2, tret);

                        if (f->isref)
                        {
                            // Function returns a reference
                            exp = exp->toLvalue(sc2, exp);
                            checkReturnEscapeRef(sc2, exp, false);
                        }
                        else
                        {
                            exp = exp->optimize(WANTvalue);

                            /* Bugzilla 10789:
                             * If NRVO is not possible, all returned lvalues should call their postblits.
                             */
                            if (!funcdecl->nrvo_can)
                                exp = doCopyOrMove(sc2, exp);

                            if (tret->hasPointers())
                                checkReturnEscape(sc2, exp, false);
                        }

                        exp = checkGC(sc2, exp);

                        if (funcdecl->vresult)
                        {
                            // Create: return vresult = exp;
                            exp = new BlitExp(rs->loc, funcdecl->vresult, exp);
                            exp->type = funcdecl->vresult->type;

                            if (rs->caseDim)
                                exp = Expression::combine(exp, new IntegerExp(rs->caseDim));
                        }
                        else if (funcdecl->tintro && !tret->equals(funcdecl->tintro->nextOf()))
                        {
                            exp = exp->implicitCastTo(sc2, funcdecl->tintro->nextOf());
                        }
                        rs->exp = exp;
                    }
                }
                if (funcdecl->nrvo_var || funcdecl->returnLabel)
                {
                    NrvoWalker nw;
                    nw.fd = funcdecl;
                    nw.sc = sc2;
                    nw.visitStmt(funcdecl->fbody);
                }

                sc2 = sc2->pop();
            }

            funcdecl->frequire = funcdecl->mergeFrequire(funcdecl->frequire);
            funcdecl->fensure = funcdecl->mergeFensure(funcdecl->fensure, Id::result);

            Statement *freq = funcdecl->frequire;
            Statement *fens = funcdecl->fensure;

            /* Do the semantic analysis on the [in] preconditions and
             * [out] postconditions.
             */
            if (freq)
            {
                /* frequire is composed of the [in] contracts
                */
                ScopeDsymbol *sym = new ScopeDsymbol();
                sym->parent = sc2->scopesym;
                sym->loc = funcdecl->loc;
                sym->endlinnum = funcdecl->endloc.linnum;
                sc2 = sc2->push(sym);
                sc2->flags = (sc2->flags & ~SCOPEcontract) | SCOPErequire;

                // BUG: need to error if accessing out parameters
                // BUG: need to disallow returns and throws
                // BUG: verify that all in and ref parameters are read
                freq = statementSemantic(freq, sc2);
                blockExit(freq, funcdecl, false);

                sc2 = sc2->pop();

                if (global.params.useIn == CHECKENABLEoff)
                    freq = NULL;
            }

            if (fens)
            {
                /* fensure is composed of the [out] contracts
                */
                if (f->next->ty == Tvoid && funcdecl->fensures)
                {
                    for (size_t i = 0; i < funcdecl->fensures->length; i++)
                    {
                        Ensure e = (*funcdecl->fensures)[i];
                        if (e.id)
                        {
                            funcdecl->error(e.ensure->loc, "`void` functions have no result");
                            //fens = NULL;
                        }
                    }
                }

                sc2 = scout;    //push
                sc2->flags = (sc2->flags & ~SCOPEcontract) | SCOPEensure;

                // BUG: need to disallow returns and throws
                if (funcdecl->fensure && f->next->ty != Tvoid)
                    funcdecl->buildResultVar(scout, f->next);

                fens = statementSemantic(fens, sc2);
                blockExit(fens, funcdecl, false);

                sc2 = sc2->pop();

                if (global.params.useOut == CHECKENABLEoff)
                    fens = NULL;
            }

            if (funcdecl->fbody && funcdecl->fbody->isErrorStatement())
                ;
            else
            {
                Statements *a = new Statements();

                // Merge in initialization of 'out' parameters
                if (funcdecl->parameters)
                {
                    for (size_t i = 0; i < funcdecl->parameters->length; i++)
                    {
                        VarDeclaration *v = (*funcdecl->parameters)[i];
                        if (v->storage_class & STCout)
                        {
                            assert(v->_init);
                            ExpInitializer *ie = v->_init->isExpInitializer();
                            assert(ie);
                            if (ie->exp->op == TOKconstruct)
                                ie->exp->op = TOKassign; // construction occured in parameter processing
                            a->push(new ExpStatement(Loc(), ie->exp));
                        }
                    }
                }

                if (funcdecl->v_argptr)
                {
                    // Handled in FuncDeclaration::toObjFile
                    funcdecl->v_argptr->_init = new VoidInitializer(funcdecl->loc);
                }

                if (_arguments)
                {
                    /* Advance to elements[] member of TypeInfo_Tuple with:
                     *  _arguments = v_arguments.elements;
                     */
                    Expression *e = new VarExp(Loc(), funcdecl->v_arguments);
                    e = new DotIdExp(Loc(), e, Id::elements);
                    e = new ConstructExp(Loc(), _arguments, e);
                    e = expressionSemantic(e, sc2);

                    _arguments->_init = new ExpInitializer(Loc(), e);
                    DeclarationExp *de = new DeclarationExp(Loc(), _arguments);
                    a->push(new ExpStatement(Loc(), de));
                }

                // Merge contracts together with body into one compound statement

                if (freq || fpreinv)
                {
                    if (!freq)
                        freq = fpreinv;
                    else if (fpreinv)
                        freq = new CompoundStatement(Loc(), freq, fpreinv);

                    a->push(freq);
                }

                if (funcdecl->fbody)
                    a->push(funcdecl->fbody);

                if (fens || fpostinv)
                {
                    if (!fens)
                        fens = fpostinv;
                    else if (fpostinv)
                        fens = new CompoundStatement(Loc(), fpostinv, fens);

                    LabelStatement *ls = new LabelStatement(Loc(), Id::returnLabel, fens);
                    funcdecl->returnLabel->statement = ls;
                    a->push(funcdecl->returnLabel->statement);

                    if (f->next->ty != Tvoid && funcdecl->vresult)
                    {
                        // Create: return vresult;
                        Expression *e = new VarExp(Loc(), funcdecl->vresult);
                        if (funcdecl->tintro)
                        {
                            e = e->implicitCastTo(sc, funcdecl->tintro->nextOf());
                            e = expressionSemantic(e, sc);
                        }
                        ReturnStatement *s = new ReturnStatement(Loc(), e);
                        a->push(s);
                    }
                }
                if (addReturn0(funcdecl))
                {
                    // Add a return 0; statement
                    Statement *s = new ReturnStatement(Loc(), new IntegerExp(0));
                    a->push(s);
                }

                Statement *sbody = new CompoundStatement(Loc(), a);
                /* Append destructor calls for parameters as finally blocks.
                */
                if (funcdecl->parameters)
                {
                    for (size_t i = 0; i < funcdecl->parameters->length; i++)
                    {
                        VarDeclaration *v = (*funcdecl->parameters)[i];

                        if (v->storage_class & (STCref | STCout | STClazy))
                            continue;

                        if (v->needsScopeDtor())
                        {
                            // same with ExpStatement.scopeCode()
                            Statement *s = new DtorExpStatement(Loc(), v->edtor, v);
                            v->storage_class |= STCnodtor;

                            s = statementSemantic(s, sc2);

                            bool isnothrow = f->isnothrow & !(funcdecl->flags & FUNCFLAGnothrowInprocess);
                            int blockexit = blockExit(s, funcdecl, isnothrow);
                            if (f->isnothrow && isnothrow && blockexit & BEthrow)
                                error(funcdecl->loc, "nothrow %s `%s` may throw", funcdecl->kind(), funcdecl->toPrettyChars());
                            if (funcdecl->flags & FUNCFLAGnothrowInprocess && blockexit & BEthrow)
                                f->isnothrow = false;
                            if (blockExit(sbody, funcdecl, f->isnothrow) == BEfallthru)
                                sbody = new CompoundStatement(Loc(), sbody, s);
                            else
                                sbody = new TryFinallyStatement(Loc(), sbody, s);
                        }
                    }
                }
                // from this point on all possible 'throwers' are checked
                funcdecl->flags &= ~FUNCFLAGnothrowInprocess;

                if (funcdecl->isSynchronized())
                {
                    /* Wrap the entire function body in a synchronized statement
                    */
                    ClassDeclaration *cd = funcdecl->isThis() ? funcdecl->isThis()->isClassDeclaration() : funcdecl->parent->isClassDeclaration();

                    if (cd)
                    {
                        if (target.libraryObjectMonitors(funcdecl, sbody))
                        {
                            Expression *vsync;
                            if (funcdecl->isStatic())
                            {
                                // The monitor is in the ClassInfo
                                vsync = new DotIdExp(funcdecl->loc, resolve(funcdecl->loc, sc2, cd, false), Id::classinfo);
                            }
                            else
                            {
                                // 'this' is the monitor
                                vsync = new VarExp(funcdecl->loc, funcdecl->vthis);
                            }
                            sbody = new PeelStatement(sbody);       // don't redo semantic()
                            sbody = new SynchronizedStatement(funcdecl->loc, vsync, sbody);
                            sbody = statementSemantic(sbody, sc2);
                        }
                    }
                    else
                    {
                        funcdecl->error("synchronized function %s must be a member of a class", funcdecl->toChars());
                    }
                }

                // If declaration has no body, don't set sbody to prevent incorrect codegen.
                if (funcdecl->fbody || allowsContractWithoutBody(funcdecl))
                    funcdecl->fbody = sbody;
            }

            // Fix up forward-referenced gotos
            if (funcdecl->gotos)
            {
                for (size_t i = 0; i < funcdecl->gotos->length; ++i)
                {
                    (*funcdecl->gotos)[i]->checkLabel();
                }
            }

            if (funcdecl->naked && (funcdecl->fensures || funcdecl->frequires))
                funcdecl->error("naked assembly functions with contracts are not supported");

            sc2->callSuper = 0;
            sc2->pop();
        }

        if (funcdecl->checkClosure())
        {
            // We should be setting errors here instead of relying on the global error count.
            //errors = true;
        }

        /* If function survived being marked as impure, then it is pure
        */
        if (funcdecl->flags & FUNCFLAGpurityInprocess)
        {
            funcdecl->flags &= ~FUNCFLAGpurityInprocess;
            if (funcdecl->type == f)
                f = (TypeFunction *)f->copy();
            f->purity = PUREfwdref;
        }

        if (funcdecl->flags & FUNCFLAGsafetyInprocess)
        {
            funcdecl->flags &= ~FUNCFLAGsafetyInprocess;
            if (funcdecl->type == f)
                f = (TypeFunction *)f->copy();
            f->trust = TRUSTsafe;
        }

        if (funcdecl->flags & FUNCFLAGnogcInprocess)
        {
            funcdecl->flags &= ~FUNCFLAGnogcInprocess;
            if (funcdecl->type == f)
                f = (TypeFunction *)f->copy();
            f->isnogc = true;
        }

        if (funcdecl->flags & FUNCFLAGreturnInprocess)
        {
            funcdecl->flags &= ~FUNCFLAGreturnInprocess;
            if (funcdecl->storage_class & STCreturn)
            {
                if (funcdecl->type == f)
                    f = (TypeFunction *)f->copy();
                f->isreturn = true;
            }
        }

        funcdecl->flags &= ~FUNCFLAGinferScope;

        // Infer STCscope
        if (funcdecl->parameters)
        {
            size_t nfparams = f->parameterList.length();
            assert(nfparams == funcdecl->parameters->length);
            for (size_t u = 0; u < funcdecl->parameters->length; u++)
            {
                VarDeclaration *v = (*funcdecl->parameters)[u];
                if (v->storage_class & STCmaybescope)
                {
                    //printf("Inferring scope for %s\n", v->toChars());
                    Parameter *p = f->parameterList[u];
                    v->storage_class &= ~STCmaybescope;
                    v->storage_class |= STCscope | STCscopeinferred;
                    p->storageClass |= STCscope | STCscopeinferred;
                    assert(!(p->storageClass & STCmaybescope));
                }
            }
        }

        if (funcdecl->vthis && funcdecl->vthis->storage_class & STCmaybescope)
        {
            funcdecl->vthis->storage_class &= ~STCmaybescope;
            funcdecl->vthis->storage_class |= STCscope | STCscopeinferred;
            f->isscope = true;
            f->isscopeinferred = true;
        }

        // reset deco to apply inference result to mangled name
        if (f != funcdecl->type)
            f->deco = NULL;

        // Do semantic type AFTER pure/nothrow inference.
        if (!f->deco && funcdecl->ident != Id::xopEquals && funcdecl->ident != Id::xopCmp)
        {
            sc = sc->push();
            if (funcdecl->isCtorDeclaration()) // Bugzilla #15665
                sc->flags |= SCOPEctor;
            sc->stc = 0;
            sc->linkage = funcdecl->linkage;  // Bugzilla 8496
            funcdecl->type = typeSemantic(f, funcdecl->loc, sc);
            sc = sc->pop();
        }

        /* If this function had instantiated with gagging, error reproduction will be
         * done by TemplateInstance::semantic.
         * Otherwise, error gagging should be temporarily ungagged by functionSemantic3.
         */
        funcdecl->semanticRun = PASSsemantic3done;
        funcdecl->semantic3Errors = (global.errors != oldErrors) || (funcdecl->fbody && funcdecl->fbody->isErrorStatement());
        if (funcdecl->type->ty == Terror)
            funcdecl->errors = true;
        //printf("-FuncDeclaration::semantic3('%s.%s', sc = %p, loc = %s)\n", funcdecl->parent->toChars(), funcdecl->toChars(), sc, funcdecl->loc.toChars());
        //fflush(stdout);
    }

    void visit(Nspace *ns)
    {
        if (ns->semanticRun >= PASSsemantic3)
            return;
        ns->semanticRun = PASSsemantic3;
        if (ns->members)
        {
            sc = sc->push(ns);
            sc->linkage = LINKcpp;
            for (size_t i = 0; i < ns->members->length; i++)
            {
                Dsymbol *s = (*ns->members)[i];
                semantic3(s, sc);
            }
            sc->pop();
        }
    }

    void visit(AttribDeclaration *ad)
    {
        Dsymbols *d = ad->include(sc);

        if (d)
        {
            Scope *sc2 = ad->newScope(sc);

            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                semantic3(s, sc2);
            }

            if (sc2 != sc)
                sc2->pop();
        }
    }

    void visit(AggregateDeclaration *ad)
    {
        //printf("AggregateDeclaration::semantic3(%s) type = %s, errors = %d\n", ad->toChars(), ad->type->toChars(), ad->errors);
        if (!ad->members)
            return;

        StructDeclaration *sd = ad->isStructDeclaration();
        if (!sc)    // from runDeferredSemantic3 for TypeInfo generation
        {
            assert(sd);
            sd->semanticTypeInfoMembers();
            return;
        }

        Scope *sc2 = ad->newScope(sc);

        for (size_t i = 0; i < ad->members->length; i++)
        {
            Dsymbol *s = (*ad->members)[i];
            semantic3(s, sc2);
        }

        sc2->pop();

        // don't do it for unused deprecated types
        // or error types
        if (!ad->getRTInfo && Type::rtinfo &&
            (!ad->isDeprecated() || global.params.useDeprecated != DIAGNOSTICerror) &&
            (ad->type && ad->type->ty != Terror))
        {
            // Evaluate: RTinfo!type
            Objects *tiargs = new Objects();
            tiargs->push(ad->type);
            TemplateInstance *ti = new TemplateInstance(ad->loc, Type::rtinfo, tiargs);

            Scope *sc3 = ti->tempdecl->_scope->startCTFE();
            sc3->tinst = sc->tinst;
            sc3->minst = sc->minst;
            if (ad->isDeprecated())
                sc3->stc |= STCdeprecated;

            dsymbolSemantic(ti, sc3);
            semantic2(ti, sc3);
            semantic3(ti, sc3);
            Expression *e = resolve(Loc(), sc3, ti->toAlias(), false);

            sc3->endCTFE();

            e = e->ctfeInterpret();
            ad->getRTInfo = e;
        }

        if (sd)
            sd->semanticTypeInfoMembers();
        ad->semanticRun = PASSsemantic3done;
    }
};

/*************************************
 * Does semantic analysis on function bodies.
 */
void semantic3(Dsymbol *dsym, Scope *sc)
{
    Semantic3Visitor v(sc);
    dsym->accept(&v);
}
