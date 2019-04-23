
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "root/dsystem.h"
#include "root/rmem.h"
#include "root/checkedint.h"

#include "errors.h"
#include "statement.h"
#include "expression.h"
#include "cond.h"
#include "init.h"
#include "staticassert.h"
#include "module.h"
#include "scope.h"
#include "declaration.h"
#include "aggregate.h"
#include "id.h"
#include "enum.h"
#include "template.h"
#include "import.h"
#include "target.h"
#include "visitor.h"

StorageClass mergeFuncAttrs(StorageClass s1, FuncDeclaration *f);
bool checkReturnEscapeRef(Scope *sc, Expression *e, bool gag);
bool checkThrowEscape(Scope *sc, Expression *e, bool gag);
LabelStatement *checkLabeledLoop(Scope *sc, Statement *statement);
Identifier *fixupLabelName(Scope *sc, Identifier *ident);
FuncDeclaration *isFuncAddress(Expression *e, bool *hasOverloads = NULL);
VarDeclaration *copyToTemp(StorageClass stc, const char *name, Expression *e);
Expression *checkAssignmentAsCondition(Expression *e);
TypeIdentifier *getThrowable();

Expression *semantic(Expression *e, Scope *sc);
Statement *semantic(Statement *s, Scope *sc);
void semantic(Catch *c, Scope *sc);
Statement *semanticNoScope(Statement *s, Scope *sc);
Statement *semanticScope(Statement *s, Scope *sc, Statement *sbreak, Statement *scontinue);
int blockExit(Statement *s, FuncDeclaration *func, bool mustNotThrow);

class StatementSemanticVisitor : public Visitor
{
public:
    Statement *result;
    Scope *sc;

    StatementSemanticVisitor(Scope *sc)
    {
        this->result = NULL;
        this->sc = sc;
    }

private:
    void setError()
    {
        result = new ErrorStatement();
    }

public:
    void visit(Statement *s)
    {
        result = s;
    }

    void visit(ErrorStatement *s)
    {
        result = s;
    }

    void visit(PeelStatement *s)
    {
        /* "peel" off this wrapper, and don't run semantic()
         * on the result.
         */
        result = s->s;
    }

    void visit(ExpStatement *s)
    {
        if (s->exp)
        {
            //printf("ExpStatement::semantic() %s\n", s->exp->toChars());

            // Allow CommaExp in ExpStatement because return isn't used
            if (s->exp->op == TOKcomma)
                ((CommaExp *)s->exp)->allowCommaExp = true;

            s->exp = semantic(s->exp, sc);
            s->exp = resolveProperties(sc, s->exp);
            s->exp = s->exp->addDtorHook(sc);
            if (FuncDeclaration *f = isFuncAddress(s->exp))
            {
                if (f->checkForwardRef(s->exp->loc))
                    s->exp = new ErrorExp();
            }
            if (discardValue(s->exp))
                s->exp = new ErrorExp();

            s->exp = s->exp->optimize(WANTvalue);
            s->exp = checkGC(sc, s->exp);
            if (s->exp->op == TOKerror)
                return setError();
        }
        result = s;
    }

    void visit(CompileStatement *cs)
    {
        //printf("CompileStatement::semantic() %s\n", cs->exp->toChars());
        Statements *a = cs->flatten(sc);
        if (!a)
            return;
        Statement *s = new CompoundStatement(cs->loc, a);
        result = semantic(s, sc);
    }

    void visit(CompoundStatement *cs)
    {
        //printf("CompoundStatement::semantic(this = %p, sc = %p)\n", cs, sc);
        for (size_t i = 0; i < cs->statements->dim; )
        {
            Statement *s = (*cs->statements)[i];
            if (s)
            {
                Statements *flt = s->flatten(sc);
                if (flt)
                {
                    cs->statements->remove(i);
                    cs->statements->insert(i, flt);
                    continue;
                }
                s = semantic(s, sc);
                (*cs->statements)[i] = s;
                if (s)
                {
                    Statement *sentry;
                    Statement *sexception;
                    Statement *sfinally;

                    (*cs->statements)[i] = s->scopeCode(sc, &sentry, &sexception, &sfinally);
                    if (sentry)
                    {
                        sentry = semantic(sentry, sc);
                        cs->statements->insert(i, sentry);
                        i++;
                    }
                    if (sexception)
                        sexception = semantic(sexception, sc);
                    if (sexception)
                    {
                        if (i + 1 == cs->statements->dim && !sfinally)
                        {
                        }
                        else
                        {
                            /* Rewrite:
                             *      s; s1; s2;
                             * As:
                             *      s;
                             *      try { s1; s2; }
                             *      catch (Throwable __o)
                             *      { sexception; throw __o; }
                             */
                            Statements *a = new Statements();
                            for (size_t j = i + 1; j < cs->statements->dim; j++)
                            {
                                a->push((*cs->statements)[j]);
                            }
                            Statement *body = new CompoundStatement(Loc(), a);
                            body = new ScopeStatement(Loc(), body, Loc());

                            Identifier *id = Identifier::generateId("__o");

                            Statement *handler = new PeelStatement(sexception);
                            if (blockExit(sexception, sc->func, false) & BEfallthru)
                            {
                                ThrowStatement *ts = new ThrowStatement(Loc(), new IdentifierExp(Loc(), id));
                                ts->internalThrow = true;
                                handler = new CompoundStatement(Loc(), handler, ts);
                            }

                            Catches *catches = new Catches();
                            Catch *ctch = new Catch(Loc(), getThrowable(), id, handler);
                            ctch->internalCatch = true;
                            catches->push(ctch);

                            s = new TryCatchStatement(Loc(), body, catches);
                            if (sfinally)
                                s = new TryFinallyStatement(Loc(), s, sfinally);
                            s = semantic(s, sc);

                            cs->statements->setDim(i + 1);
                            cs->statements->push(s);
                            break;
                        }
                    }
                    else if (sfinally)
                    {
                        if (0 && i + 1 == cs->statements->dim)
                        {
                            cs->statements->push(sfinally);
                        }
                        else
                        {
                            /* Rewrite:
                             *      s; s1; s2;
                             * As:
                             *      s; try { s1; s2; } finally { sfinally; }
                             */
                            Statements *a = new Statements();
                            for (size_t j = i + 1; j < cs->statements->dim; j++)
                            {
                                a->push((*cs->statements)[j]);
                            }
                            Statement *body = new CompoundStatement(Loc(), a);
                            s = new TryFinallyStatement(Loc(), body, sfinally);
                            s = semantic(s, sc);
                            cs->statements->setDim(i + 1);
                            cs->statements->push(s);
                            break;
                        }
                    }
                }
                else
                {
                    /* Remove NULL statements from the list.
                    */
                    cs->statements->remove(i);
                    continue;
                }
            }
            i++;
        }
        for (size_t i = 0; i < cs->statements->dim; ++i)
        {
        Lagain:
            Statement *s = (*cs->statements)[i];
            if (!s)
                continue;

            Statement *se = s->isErrorStatement();
            if (se)
            {
                result = se;
                return;
            }

            /* Bugzilla 11653: 'semantic' may return another CompoundStatement
             * (eg. CaseRangeStatement), so flatten it here.
             */
            Statements *flt = s->flatten(sc);
            if (flt)
            {
                cs->statements->remove(i);
                cs->statements->insert(i, flt);
                if (cs->statements->dim <= i)
                    break;
                goto Lagain;
            }
        }
        if (cs->statements->dim == 1)
        {
            result = (*cs->statements)[0];
            return;
        }
        result = cs;
    }

    void visit(UnrolledLoopStatement *uls)
    {
        //printf("UnrolledLoopStatement::semantic(this = %p, sc = %p)\n", uls, sc);
        Scope *scd = sc->push();
        scd->sbreak = uls;
        scd->scontinue = uls;

        Statement *serror = NULL;
        for (size_t i = 0; i < uls->statements->dim; i++)
        {
            Statement *s = (*uls->statements)[i];
            if (s)
            {
                //printf("[%d]: %s\n", i, s->toChars());
                s = semantic(s, scd);
                (*uls->statements)[i] = s;

                if (s && !serror)
                    serror = s->isErrorStatement();
            }
        }

        scd->pop();
        result = serror ? serror : uls;
    }

    void visit(ScopeStatement *ss)
    {
        ScopeDsymbol *sym;
        //printf("ScopeStatement::semantic(sc = %p)\n", sc);
        if (ss->statement)
        {
            sym = new ScopeDsymbol();
            sym->parent = sc->scopesym;
            sym->endlinnum = ss->endloc.linnum;
            sc = sc->push(sym);

            Statements *a = ss->statement->flatten(sc);
            if (a)
            {
                ss->statement = new CompoundStatement(ss->loc, a);
            }

            ss->statement = semantic(ss->statement, sc);
            if (ss->statement)
            {
                if (ss->statement->isErrorStatement())
                {
                    sc->pop();
                    result = ss->statement;
                    return;
                }

                Statement *sentry;
                Statement *sexception;
                Statement *sfinally;

                ss->statement = ss->statement->scopeCode(sc, &sentry, &sexception, &sfinally);
                assert(!sentry);
                assert(!sexception);
                if (sfinally)
                {
                    //printf("adding sfinally\n");
                    sfinally = semantic(sfinally, sc);
                    ss->statement = new CompoundStatement(ss->loc, ss->statement, sfinally);
                }
            }

            sc->pop();
        }
        result = ss;
    }

    void visit(WhileStatement *ws)
    {
        /* Rewrite as a for(;condition;) loop
         */
        Statement *s = new ForStatement(ws->loc, NULL, ws->condition, NULL, ws->_body, ws->endloc);
        s = semantic(s, sc);
        result = s;
    }

    void visit(DoStatement *ds)
    {
        sc->noctor++;
        if (ds->_body)
            ds->_body = semanticScope(ds->_body, sc, ds, ds);
        sc->noctor--;

        if (ds->condition->op == TOKdotid)
            ((DotIdExp *)ds->condition)->noderef = true;

        // check in syntax level
        ds->condition = checkAssignmentAsCondition(ds->condition);

        ds->condition = semantic(ds->condition, sc);
        ds->condition = resolveProperties(sc, ds->condition);
        ds->condition = ds->condition->optimize(WANTvalue);
        ds->condition = checkGC(sc, ds->condition);

        ds->condition = ds->condition->toBoolean(sc);

        if (ds->condition->op == TOKerror)
            return setError();

        if (ds->_body && ds->_body->isErrorStatement())
        {
            result = ds->_body;
            return;
        }

        result = ds;
    }

    void visit(ForStatement *fs)
    {
        //printf("ForStatement::semantic %s\n", toChars());

        if (fs->_init)
        {
            /* Rewrite:
             *  for (auto v1 = i1, v2 = i2; condition; increment) { ... }
             * to:
             *  { auto v1 = i1, v2 = i2; for (; condition; increment) { ... } }
             * then lowered to:
             *  auto v1 = i1;
             *  try {
             *    auto v2 = i2;
             *    try {
             *      for (; condition; increment) { ... }
             *    } finally { v2.~this(); }
             *  } finally { v1.~this(); }
             */
            Statements *ainit = new Statements();
            ainit->push(fs->_init);
            fs->_init = NULL;
            ainit->push(fs);
            Statement *s = new CompoundStatement(fs->loc, ainit);
            s = new ScopeStatement(fs->loc, s, fs->endloc);
            s = semantic(s, sc);
            if (!s->isErrorStatement())
            {
                if (LabelStatement *ls = checkLabeledLoop(sc, fs))
                    ls->gotoTarget = fs;
                fs->relatedLabeled = s;
            }
            result = s;
            return;
        }
        assert(fs->_init == NULL);

        ScopeDsymbol *sym = new ScopeDsymbol();
        sym->parent = sc->scopesym;
        sym->endlinnum = fs->endloc.linnum;
        sc = sc->push(sym);

        sc->noctor++;
        if (fs->condition)
        {
            if (fs->condition->op == TOKdotid)
                ((DotIdExp *)fs->condition)->noderef = true;

            // check in syntax level
            fs->condition = checkAssignmentAsCondition(fs->condition);

            fs->condition = semantic(fs->condition, sc);
            fs->condition = resolveProperties(sc, fs->condition);
            fs->condition = fs->condition->optimize(WANTvalue);
            fs->condition = checkGC(sc, fs->condition);
            fs->condition = fs->condition->toBoolean(sc);
        }
        if (fs->increment)
        {
            if (fs->increment->op == TOKcomma)
                ((CommaExp *)fs->increment)->allowCommaExp = true;
            fs->increment = semantic(fs->increment, sc);
            fs->increment = resolveProperties(sc, fs->increment);
            fs->increment = fs->increment->optimize(WANTvalue);
            fs->increment = checkGC(sc, fs->increment);
        }

        sc->sbreak = fs;
        sc->scontinue = fs;
        if (fs->_body)
            fs->_body = semanticNoScope(fs->_body, sc);
        sc->noctor--;

        sc->pop();

        if ((fs->condition && fs->condition->op == TOKerror) ||
            (fs->increment && fs->increment->op == TOKerror) ||
            (fs->_body && fs->_body->isErrorStatement()))
            return setError();

        result = fs;
    }

    void visit(ForeachStatement *fs)
    {
        //printf("ForeachStatement::semantic() %p\n", fs);
        ScopeDsymbol *sym;
        Statement *s = fs;
        Loc loc = fs->loc;
        size_t dim = fs->parameters->dim;
        TypeAArray *taa = NULL;
        Dsymbol *sapply = NULL;

        Type *tn = NULL;
        Type *tnv = NULL;

        fs->func = sc->func;
        if (fs->func->fes)
            fs->func = fs->func->fes->func;

        VarDeclaration *vinit = NULL;
        fs->aggr = semantic(fs->aggr, sc);
        fs->aggr = resolveProperties(sc, fs->aggr);
        fs->aggr = fs->aggr->optimize(WANTvalue);
        if (fs->aggr->op == TOKerror)
            return setError();

        Expression *oaggr = fs->aggr;
        if (fs->aggr->type && fs->aggr->type->toBasetype()->ty == Tstruct &&
            ((TypeStruct *)(fs->aggr->type->toBasetype()))->sym->dtor &&
            fs->aggr->op != TOKtype && !fs->aggr->isLvalue())
        {
            // Bugzilla 14653: Extend the life of rvalue aggregate till the end of foreach.
            vinit = copyToTemp(STCrvalue, "__aggr", fs->aggr);
            vinit->semantic(sc);
            fs->aggr = new VarExp(fs->aggr->loc, vinit);
        }

        if (!inferAggregate(fs, sc, sapply))
        {
            const char *msg = "";
            if (fs->aggr->type && isAggregate(fs->aggr->type))
            {
                msg = ", define opApply(), range primitives, or use .tupleof";
            }
            fs->error("invalid foreach aggregate %s%s", oaggr->toChars(), msg);
            return setError();
        }

        Dsymbol* sapplyOld = sapply;  // 'sapply' will be NULL if and after 'inferApplyArgTypes' errors

        /* Check for inference errors
        */
        if (!inferApplyArgTypes(fs, sc, sapply))
        {
            /**
              Try and extract the parameter count of the opApply callback function, e.g.:
              int opApply(int delegate(int, float)) => 2 args
              */
            bool foundMismatch = false;
            size_t foreachParamCount = 0;
            if (sapplyOld)
            {
                if (FuncDeclaration *fd = sapplyOld->isFuncDeclaration())
                {
                    int fvarargs;  // ignored (opApply shouldn't take variadics)
                    Parameters *fparameters = fd->getParameters(&fvarargs);

                    if (Parameter::dim(fparameters) == 1)
                    {
                        // first param should be the callback function
                        Parameter *fparam = Parameter::getNth(fparameters, 0);
                        if ((fparam->type->ty == Tpointer || fparam->type->ty == Tdelegate) &&
                            fparam->type->nextOf()->ty == Tfunction)
                        {
                            TypeFunction *tf = (TypeFunction *)fparam->type->nextOf();
                            foreachParamCount = Parameter::dim(tf->parameters);
                            foundMismatch = true;
                        }
                    }
                }
            }

            //printf("dim = %d, parameters->dim = %d\n", dim, fs->parameters->dim);
            if (foundMismatch && dim != foreachParamCount)
            {
                const char *plural = foreachParamCount > 1 ? "s" : "";
                fs->error("cannot infer argument types, expected %d argument%s, not %d",
                          foreachParamCount, plural, dim);
            }
            else
                fs->error("cannot uniquely infer foreach argument types");

            return setError();
        }

        Type *tab = fs->aggr->type->toBasetype();

        if (tab->ty == Ttuple)      // don't generate new scope for tuple loops
        {
            if (dim < 1 || dim > 2)
            {
                fs->error("only one (value) or two (key,value) arguments for tuple foreach");
                return setError();
            }

            Type *paramtype = (*fs->parameters)[dim-1]->type;
            if (paramtype)
            {
                paramtype = paramtype->semantic(loc, sc);
                if (paramtype->ty == Terror)
                    return setError();
            }

            TypeTuple *tuple = (TypeTuple *)tab;
            Statements *statements = new Statements();
            //printf("aggr: op = %d, %s\n", fs->aggr->op, fs->aggr->toChars());
            size_t n;
            TupleExp *te = NULL;
            if (fs->aggr->op == TOKtuple)       // expression tuple
            {
                te = (TupleExp *)fs->aggr;
                n = te->exps->dim;
            }
            else if (fs->aggr->op == TOKtype)   // type tuple
            {
                n = Parameter::dim(tuple->arguments);
            }
            else
                assert(0);
            for (size_t j = 0; j < n; j++)
            {
                size_t k = (fs->op == TOKforeach) ? j : n - 1 - j;
                Expression *e = NULL;
                Type *t = NULL;
                if (te)
                    e = (*te->exps)[k];
                else
                    t = Parameter::getNth(tuple->arguments, k)->type;
                Parameter *p = (*fs->parameters)[0];
                Statements *st = new Statements();

                if (dim == 2)
                {
                    // Declare key
                    if (p->storageClass & (STCout | STCref | STClazy))
                    {
                        fs->error("no storage class for key %s", p->ident->toChars());
                        return setError();
                    }
                    p->type = p->type->semantic(loc, sc);
                    TY keyty = p->type->ty;
                    if (keyty != Tint32 && keyty != Tuns32)
                    {
                        if (global.params.isLP64)
                        {
                            if (keyty != Tint64 && keyty != Tuns64)
                            {
                                fs->error("foreach: key type must be int or uint, long or ulong, not %s", p->type->toChars());
                                return setError();
                            }
                        }
                        else
                        {
                            fs->error("foreach: key type must be int or uint, not %s", p->type->toChars());
                            return setError();
                        }
                    }
                    Initializer *ie = new ExpInitializer(Loc(), new IntegerExp(k));
                    VarDeclaration *var = new VarDeclaration(loc, p->type, p->ident, ie);
                    var->storage_class |= STCmanifest;
                    st->push(new ExpStatement(loc, var));
                    p = (*fs->parameters)[1];  // value
                }
                // Declare value
                if (p->storageClass & (STCout | STClazy) ||
                    (p->storageClass & STCref && !te))
                {
                    fs->error("no storage class for value %s", p->ident->toChars());
                    return setError();
                }
                Dsymbol *var;
                if (te)
                {
                    Type *tb = e->type->toBasetype();
                    Dsymbol *ds = NULL;
                    if ((tb->ty == Tfunction || tb->ty == Tsarray) && e->op == TOKvar)
                        ds = ((VarExp *)e)->var;
                    else if (e->op == TOKtemplate)
                        ds = ((TemplateExp *)e)->td;
                    else if (e->op == TOKscope)
                        ds = ((ScopeExp *)e)->sds;
                    else if (e->op == TOKfunction)
                    {
                        FuncExp *fe = (FuncExp *)e;
                        ds = fe->td ? (Dsymbol *)fe->td : fe->fd;
                    }

                    if (ds)
                    {
                        var = new AliasDeclaration(loc, p->ident, ds);
                        if (p->storageClass & STCref)
                        {
                            fs->error("symbol %s cannot be ref", s->toChars());
                            return setError();
                        }
                        if (paramtype)
                        {
                            fs->error("cannot specify element type for symbol %s", ds->toChars());
                            return setError();
                        }
                    }
                    else if (e->op == TOKtype)
                    {
                        var = new AliasDeclaration(loc, p->ident, e->type);
                        if (paramtype)
                        {
                            fs->error("cannot specify element type for type %s", e->type->toChars());
                            return setError();
                        }
                    }
                    else
                    {
                        p->type = e->type;
                        if (paramtype)
                            p->type = paramtype;
                        Initializer *ie = new ExpInitializer(Loc(), e);
                        VarDeclaration *v = new VarDeclaration(loc, p->type, p->ident, ie);
                        if (p->storageClass & STCref)
                            v->storage_class |= STCref | STCforeach;
                        if (e->isConst() || e->op == TOKstring ||
                            e->op == TOKstructliteral || e->op == TOKarrayliteral)
                        {
                            if (v->storage_class & STCref)
                            {
                                fs->error("constant value %s cannot be ref", ie->toChars());
                                return setError();
                            }
                            else
                                v->storage_class |= STCmanifest;
                        }
                        var = v;
                    }
                }
                else
                {
                    var = new AliasDeclaration(loc, p->ident, t);
                    if (paramtype)
                    {
                        fs->error("cannot specify element type for symbol %s", s->toChars());
                        return setError();
                    }
                }
                st->push(new ExpStatement(loc, var));

                if (fs->_body)
                    st->push(fs->_body->syntaxCopy());
                s = new CompoundStatement(loc, st);
                s = new ScopeStatement(loc, s, fs->endloc);
                statements->push(s);
            }

            s = new UnrolledLoopStatement(loc, statements);
            if (LabelStatement *ls = checkLabeledLoop(sc, fs))
                ls->gotoTarget = s;
            if (te && te->e0)
                s = new CompoundStatement(loc, new ExpStatement(te->e0->loc, te->e0), s);
            if (vinit)
                s = new CompoundStatement(loc, new ExpStatement(loc, vinit), s);
            s = semantic(s, sc);
            result = s;
            return;
        }

        sym = new ScopeDsymbol();
        sym->parent = sc->scopesym;
        sym->endlinnum = fs->endloc.linnum;
        Scope *sc2 = sc->push(sym);

        sc2->noctor++;

        switch (tab->ty)
        {
            case Tarray:
            case Tsarray:
                {
                    if (fs->checkForArgTypes())
                    {
                        result = fs;
                        return;
                    }

                    if (dim < 1 || dim > 2)
                    {
                        fs->error("only one or two arguments for array foreach");
                        goto Lerror2;
                    }

                    /* Look for special case of parsing char types out of char type
                     * array.
                     */
                    tn = tab->nextOf()->toBasetype();
                    if (tn->ty == Tchar || tn->ty == Twchar || tn->ty == Tdchar)
                    {
                        int i = (dim == 1) ? 0 : 1;     // index of value
                        Parameter *p = (*fs->parameters)[i];
                        p->type = p->type->semantic(loc, sc2);
                        p->type = p->type->addStorageClass(p->storageClass);
                        tnv = p->type->toBasetype();
                        if (tnv->ty != tn->ty &&
                            (tnv->ty == Tchar || tnv->ty == Twchar || tnv->ty == Tdchar))
                        {
                            if (p->storageClass & STCref)
                            {
                                fs->error("foreach: value of UTF conversion cannot be ref");
                                goto Lerror2;
                            }
                            if (dim == 2)
                            {
                                p = (*fs->parameters)[0];
                                if (p->storageClass & STCref)
                                {
                                    fs->error("foreach: key cannot be ref");
                                    goto Lerror2;
                                }
                            }
                            goto Lapply;
                        }
                    }

                    for (size_t i = 0; i < dim; i++)
                    {
                        // Declare parameterss
                        Parameter *p = (*fs->parameters)[i];
                        p->type = p->type->semantic(loc, sc2);
                        p->type = p->type->addStorageClass(p->storageClass);
                        VarDeclaration *var;

                        if (dim == 2 && i == 0)
                        {
                            var = new VarDeclaration(loc, p->type->mutableOf(), Identifier::generateId("__key"), NULL);
                            var->storage_class |= STCtemp | STCforeach;
                            if (var->storage_class & (STCref | STCout))
                                var->storage_class |= STCnodtor;

                            fs->key = var;
                            if (p->storageClass & STCref)
                            {
                                if (var->type->constConv(p->type) <= MATCHnomatch)
                                {
                                    fs->error("key type mismatch, %s to ref %s",
                                              var->type->toChars(), p->type->toChars());
                                    goto Lerror2;
                                }
                            }
                            if (tab->ty == Tsarray)
                            {
                                TypeSArray *ta =  (TypeSArray *)tab;
                                IntRange dimrange = getIntRange(ta->dim);
                                if (!IntRange::fromType(var->type).contains(dimrange))
                                {
                                    fs->error("index type '%s' cannot cover index range 0..%llu", p->type->toChars(), ta->dim->toInteger());
                                    goto Lerror2;
                                }
                                fs->key->range = new IntRange(SignExtendedNumber(0), dimrange.imax);
                            }
                        }
                        else
                        {
                            var = new VarDeclaration(loc, p->type, p->ident, NULL);
                            var->storage_class |= STCforeach;
                            var->storage_class |= p->storageClass & (STCin | STCout | STCref | STC_TYPECTOR);
                            if (var->storage_class & (STCref | STCout))
                                var->storage_class |= STCnodtor;

                            fs->value = var;
                            if (var->storage_class & STCref)
                            {
                                if (fs->aggr->checkModifiable(sc2, 1) == 2)
                                    var->storage_class |= STCctorinit;

                                Type *t = tab->nextOf();
                                if (t->constConv(p->type) <= MATCHnomatch)
                                {
                                    fs->error("argument type mismatch, %s to ref %s",
                                              t->toChars(), p->type->toChars());
                                    goto Lerror2;
                                }
                            }
                        }
                    }

                    /* Convert to a ForStatement
                     *   foreach (key, value; a) body =>
                     *   for (T[] tmp = a[], size_t key; key < tmp.length; ++key)
                     *   { T value = tmp[k]; body }
                     *
                     *   foreach_reverse (key, value; a) body =>
                     *   for (T[] tmp = a[], size_t key = tmp.length; key--; )
                     *   { T value = tmp[k]; body }
                     */
                    Identifier *id = Identifier::generateId("__r");
                    ExpInitializer *ie = new ExpInitializer(loc, new SliceExp(loc, fs->aggr, NULL, NULL));
                    VarDeclaration *tmp;
                    if (fs->aggr->op == TOKarrayliteral &&
                        !((*fs->parameters)[dim - 1]->storageClass & STCref))
                    {
                        ArrayLiteralExp *ale = (ArrayLiteralExp *)fs->aggr;
                        size_t edim = ale->elements ? ale->elements->dim : 0;
                        Type *telem = (*fs->parameters)[dim - 1]->type;

                        // Bugzilla 12936: if telem has been specified explicitly,
                        // converting array literal elements to telem might make it @nogc.
                        fs->aggr = fs->aggr->implicitCastTo(sc, telem->sarrayOf(edim));
                        if (fs->aggr->op == TOKerror)
                            goto Lerror2;

                        // for (T[edim] tmp = a, ...)
                        tmp = new VarDeclaration(loc, fs->aggr->type, id, ie);
                    }
                    else
                        tmp = new VarDeclaration(loc, tab->nextOf()->arrayOf(), id, ie);
                    tmp->storage_class |= STCtemp;
                    tmp->endlinnum = fs->endloc.linnum;

                    Expression *tmp_length = new DotIdExp(loc, new VarExp(loc, tmp), Id::length);

                    if (!fs->key)
                    {
                        Identifier *idkey = Identifier::generateId("__key");
                        fs->key = new VarDeclaration(loc, Type::tsize_t, idkey, NULL);
                        fs->key->storage_class |= STCtemp;
                    }
                    if (fs->op == TOKforeach_reverse)
                        fs->key->_init = new ExpInitializer(loc, tmp_length);
                    else
                        fs->key->_init = new ExpInitializer(loc, new IntegerExp(loc, 0, fs->key->type));

                    Statements *cs = new Statements();
                    if (vinit)
                        cs->push(new ExpStatement(loc, vinit));
                    cs->push(new ExpStatement(loc, tmp));
                    cs->push(new ExpStatement(loc, fs->key));
                    Statement *forinit = new CompoundDeclarationStatement(loc, cs);

                    Expression *cond;
                    if (fs->op == TOKforeach_reverse)
                    {
                        // key--
                        cond = new PostExp(TOKminusminus, loc, new VarExp(loc, fs->key));
                    }
                    else
                    {
                        // key < tmp.length
                        cond = new CmpExp(TOKlt, loc, new VarExp(loc, fs->key), tmp_length);
                    }

                    Expression *increment = NULL;
                    if (fs->op == TOKforeach)
                    {
                        // key += 1
                        increment = new AddAssignExp(loc, new VarExp(loc, fs->key), new IntegerExp(loc, 1, fs->key->type));
                    }

                    // T value = tmp[key];
                    fs->value->_init = new ExpInitializer(loc, new IndexExp(loc, new VarExp(loc, tmp), new VarExp(loc, fs->key)));
                    Statement *ds = new ExpStatement(loc, fs->value);

                    if (dim == 2)
                    {
                        Parameter *p = (*fs->parameters)[0];
                        if ((p->storageClass & STCref) && p->type->equals(fs->key->type))
                        {
                            fs->key->range = NULL;
                            AliasDeclaration *v = new AliasDeclaration(loc, p->ident, fs->key);
                            fs->_body = new CompoundStatement(loc, new ExpStatement(loc, v), fs->_body);
                        }
                        else
                        {
                            ExpInitializer *ei = new ExpInitializer(loc, new IdentifierExp(loc, fs->key->ident));
                            VarDeclaration *v = new VarDeclaration(loc, p->type, p->ident, ei);
                            v->storage_class |= STCforeach | (p->storageClass & STCref);
                            fs->_body = new CompoundStatement(loc, new ExpStatement(loc, v), fs->_body);
                            if (fs->key->range && !p->type->isMutable())
                            {
                                /* Limit the range of the key to the specified range
                                */
                                v->range = new IntRange(fs->key->range->imin, fs->key->range->imax - SignExtendedNumber(1));
                            }
                        }
                    }
                    fs->_body = new CompoundStatement(loc, ds, fs->_body);

                    s = new ForStatement(loc, forinit, cond, increment, fs->_body, fs->endloc);
                    if (LabelStatement *ls = checkLabeledLoop(sc, fs))   // Bugzilla 15450: don't use sc2
                        ls->gotoTarget = s;
                    s = semantic(s, sc2);
                    break;
                }

            case Taarray:
                if (fs->op == TOKforeach_reverse)
                    fs->warning("cannot use foreach_reverse with an associative array");
                if (fs->checkForArgTypes())
                {
                    result = fs;
                    return;
                }

                taa = (TypeAArray *)tab;
                if (dim < 1 || dim > 2)
                {
                    fs->error("only one or two arguments for associative array foreach");
                    goto Lerror2;
                }
                goto Lapply;

            case Tclass:
            case Tstruct:
                /* Prefer using opApply, if it exists
                */
                if (sapply)
                    goto Lapply;

                {
                    /* Look for range iteration, i.e. the properties
                     * .empty, .popFront, .popBack, .front and .back
                     *    foreach (e; aggr) { ... }
                     * translates to:
                     *    for (auto __r = aggr[]; !__r.empty; __r.popFront()) {
                     *        auto e = __r.front;
                     *        ...
                     *    }
                     */
                    AggregateDeclaration *ad = (tab->ty == Tclass)
                        ? (AggregateDeclaration *)((TypeClass  *)tab)->sym
                        : (AggregateDeclaration *)((TypeStruct *)tab)->sym;
                    Identifier *idfront;
                    Identifier *idpopFront;
                    if (fs->op == TOKforeach)
                    {
                        idfront = Id::Ffront;
                        idpopFront = Id::FpopFront;
                    }
                    else
                    {
                        idfront = Id::Fback;
                        idpopFront = Id::FpopBack;
                    }
                    Dsymbol *sfront = ad->search(Loc(), idfront);
                    if (!sfront)
                        goto Lapply;

                    /* Generate a temporary __r and initialize it with the aggregate.
                     */
                    VarDeclaration *r;
                    Statement *init;
                    if (vinit && fs->aggr->op == TOKvar && ((VarExp *)fs->aggr)->var == vinit)
                    {
                        r = vinit;
                        init = new ExpStatement(loc, vinit);
                    }
                    else
                    {
                        r = copyToTemp(0, "__r", fs->aggr);
                        init = new ExpStatement(loc, r);
                        if (vinit)
                            init = new CompoundStatement(loc, new ExpStatement(loc, vinit), init);
                    }

                    // !__r.empty
                    Expression *e = new VarExp(loc, r);
                    e = new DotIdExp(loc, e, Id::Fempty);
                    Expression *condition = new NotExp(loc, e);

                    // __r.idpopFront()
                    e = new VarExp(loc, r);
                    Expression *increment = new CallExp(loc, new DotIdExp(loc, e, idpopFront));

                    /* Declaration statement for e:
                     *    auto e = __r.idfront;
                     */
                    e = new VarExp(loc, r);
                    Expression *einit = new DotIdExp(loc, e, idfront);
                    Statement *makeargs, *forbody;
                    if (dim == 1)
                    {
                        Parameter *p = (*fs->parameters)[0];
                        VarDeclaration *ve = new VarDeclaration(loc, p->type, p->ident, new ExpInitializer(loc, einit));
                        ve->storage_class |= STCforeach;
                        ve->storage_class |= p->storageClass & (STCin | STCout | STCref | STC_TYPECTOR);

                        makeargs = new ExpStatement(loc, ve);
                    }
                    else
                    {
                        VarDeclaration *vd = copyToTemp(STCref, "__front", einit);
                        makeargs = new ExpStatement(loc, vd);

                        Type *tfront = NULL;
                        if (FuncDeclaration *fd = sfront->isFuncDeclaration())
                        {
                            if (!fd->functionSemantic())
                                goto Lrangeerr;
                            tfront = fd->type;
                        }
                        else if (TemplateDeclaration *td = sfront->isTemplateDeclaration())
                        {
                            Expressions a;
                            if (FuncDeclaration *f = resolveFuncCall(loc, sc, td, NULL, tab, &a, 1))
                                tfront = f->type;
                        }
                        else if (Declaration *d = sfront->isDeclaration())
                        {
                            tfront = d->type;
                        }
                        if (!tfront || tfront->ty == Terror)
                            goto Lrangeerr;

                        if (tfront->toBasetype()->ty == Tfunction)
                            tfront = tfront->toBasetype()->nextOf();
                        if (tfront->ty == Tvoid)
                        {
                            fs->error("%s.front is void and has no value", oaggr->toChars());
                            goto Lerror2;
                        }

                        // Resolve inout qualifier of front type
                        tfront = tfront->substWildTo(tab->mod);

                        Expression *ve = new VarExp(loc, vd);
                        ve->type = tfront;

                        Expressions *exps = new Expressions();
                        exps->push(ve);
                        int pos = 0;
                        while (exps->dim < dim)
                        {
                            pos = expandAliasThisTuples(exps, pos);
                            if (pos == -1)
                                break;
                        }
                        if (exps->dim != dim)
                        {
                            const char *plural = exps->dim > 1 ? "s" : "";
                            fs->error("cannot infer argument types, expected %d argument%s, not %d",
                                      exps->dim, plural, dim);
                            goto Lerror2;
                        }

                        for (size_t i = 0; i < dim; i++)
                        {
                            Parameter *p = (*fs->parameters)[i];
                            Expression *exp = (*exps)[i];
                            if (!p->type)
                                p->type = exp->type;
                            p->type = p->type->addStorageClass(p->storageClass)->semantic(loc, sc2);
                            if (!exp->implicitConvTo(p->type))
                                goto Lrangeerr;

                            VarDeclaration *var = new VarDeclaration(loc, p->type, p->ident, new ExpInitializer(loc, exp));
                            var->storage_class |= STCctfe | STCref | STCforeach;
                            makeargs = new CompoundStatement(loc, makeargs, new ExpStatement(loc, var));
                        }

                    }

                    forbody = new CompoundStatement(loc,
                                                    makeargs, fs->_body);

                    s = new ForStatement(loc, init, condition, increment, forbody, fs->endloc);
                    if (LabelStatement *ls = checkLabeledLoop(sc, fs))
                        ls->gotoTarget = s;
                    s = semantic(s, sc2);
                    break;

                Lrangeerr:
                    fs->error("cannot infer argument types");
                    goto Lerror2;
                }
            case Tdelegate:
                if (fs->op == TOKforeach_reverse)
                    fs->deprecation("cannot use foreach_reverse with a delegate");
            Lapply:
                {
                    if (fs->checkForArgTypes())
                    {
                        fs->_body = semanticNoScope(fs->_body, sc2);
                        result = fs;
                        return;
                    }

                    TypeFunction *tfld = NULL;
                    if (sapply)
                    {
                        FuncDeclaration *fdapply = sapply->isFuncDeclaration();
                        if (fdapply)
                        {
                            assert(fdapply->type && fdapply->type->ty == Tfunction);
                            tfld = (TypeFunction *)fdapply->type->semantic(loc, sc2);
                            goto Lget;
                        }
                        else if (tab->ty == Tdelegate)
                        {
                            tfld = (TypeFunction *)tab->nextOf();
                        Lget:
                            //printf("tfld = %s\n", tfld->toChars());
                            if (tfld->parameters->dim == 1)
                            {
                                Parameter *p = Parameter::getNth(tfld->parameters, 0);
                                if (p->type && p->type->ty == Tdelegate)
                                {
                                    Type *t = p->type->semantic(loc, sc2);
                                    assert(t->ty == Tdelegate);
                                    tfld = (TypeFunction *)t->nextOf();
                                }
                            }
                        }
                    }

                    /* Turn body into the function literal:
                     *  int delegate(ref T param) { body }
                     */
                    Parameters *params = new Parameters();
                    for (size_t i = 0; i < dim; i++)
                    {
                        Parameter *p = (*fs->parameters)[i];
                        StorageClass stc = STCref;
                        Identifier *id;

                        p->type = p->type->semantic(loc, sc2);
                        p->type = p->type->addStorageClass(p->storageClass);
                        if (tfld)
                        {
                            Parameter *prm = Parameter::getNth(tfld->parameters, i);
                            //printf("\tprm = %s%s\n", (prm->storageClass&STCref?"ref ":""), prm->ident->toChars());
                            stc = prm->storageClass & STCref;
                            id = p->ident;    // argument copy is not need.
                            if ((p->storageClass & STCref) != stc)
                            {
                                if (!stc)
                                {
                                    fs->error("foreach: cannot make %s ref", p->ident->toChars());
                                    goto Lerror2;
                                }
                                goto LcopyArg;
                            }
                        }
                        else if (p->storageClass & STCref)
                        {
                            // default delegate parameters are marked as ref, then
                            // argument copy is not need.
                            id = p->ident;
                        }
                        else
                        {
                            // Make a copy of the ref argument so it isn't
                            // a reference.
                        LcopyArg:
                            id = Identifier::generateId("__applyArg", (int)i);

                            Initializer *ie = new ExpInitializer(Loc(), new IdentifierExp(Loc(), id));
                            VarDeclaration *v = new VarDeclaration(Loc(), p->type, p->ident, ie);
                            v->storage_class |= STCtemp;
                            s = new ExpStatement(Loc(), v);
                            fs->_body = new CompoundStatement(loc, s, fs->_body);
                        }
                        params->push(new Parameter(stc, p->type, id, NULL));
                    }
                    // Bugzilla 13840: Throwable nested function inside nothrow function is acceptable.
                    StorageClass stc = mergeFuncAttrs(STCsafe | STCpure | STCnogc, fs->func);
                    tfld = new TypeFunction(params, Type::tint32, 0, LINKd, stc);
                    fs->cases = new Statements();
                    fs->gotos = new ScopeStatements();
                    FuncLiteralDeclaration *fld = new FuncLiteralDeclaration(loc, Loc(), tfld, TOKdelegate, fs);
                    fld->fbody = fs->_body;
                    Expression *flde = new FuncExp(loc, fld);
                    flde = semantic(flde, sc2);
                    fld->tookAddressOf = 0;

                    // Resolve any forward referenced goto's
                    for (size_t i = 0; i < fs->gotos->dim; i++)
                    {
                        GotoStatement *gs = (GotoStatement *)(*fs->gotos)[i]->statement;
                        if (!gs->label->statement)
                        {
                            // 'Promote' it to this scope, and replace with a return
                            fs->cases->push(gs);
                            s = new ReturnStatement(Loc(), new IntegerExp(fs->cases->dim + 1));
                            (*fs->gotos)[i]->statement = s;
                        }
                    }

                    Expression *e = NULL;
                    Expression *ec;
                    if (vinit)
                    {
                        e = new DeclarationExp(loc, vinit);
                        e = semantic(e, sc2);
                        if (e->op == TOKerror)
                            goto Lerror2;
                    }

                    if (taa)
                    {
                        // Check types
                        Parameter *p = (*fs->parameters)[0];
                        bool isRef = (p->storageClass & STCref) != 0;
                        Type *ta = p->type;
                        if (dim == 2)
                        {
                            Type *ti = (isRef ? taa->index->addMod(MODconst) : taa->index);
                            if (isRef ? !ti->constConv(ta) : !ti->implicitConvTo(ta))
                            {
                                fs->error("foreach: index must be type %s, not %s", ti->toChars(), ta->toChars());
                                goto Lerror2;
                            }
                            p = (*fs->parameters)[1];
                            isRef = (p->storageClass & STCref) != 0;
                            ta = p->type;
                        }
                        Type *taav = taa->nextOf();
                        if (isRef ? !taav->constConv(ta) : !taav->implicitConvTo(ta))
                        {
                            fs->error("foreach: value must be type %s, not %s", taav->toChars(), ta->toChars());
                            goto Lerror2;
                        }

                        /* Call:
                         *  extern(C) int _aaApply(void*, in size_t, int delegate(void*))
                         *      _aaApply(aggr, keysize, flde)
                         *
                         *  extern(C) int _aaApply2(void*, in size_t, int delegate(void*, void*))
                         *      _aaApply2(aggr, keysize, flde)
                         */
                        static const char *name[2] = { "_aaApply", "_aaApply2" };
                        static FuncDeclaration *fdapply[2] = { NULL, NULL };
                        static TypeDelegate *fldeTy[2] = { NULL, NULL };

                        unsigned char i = (dim == 2 ? 1 : 0);
                        if (!fdapply[i])
                        {
                            params = new Parameters();
                            params->push(new Parameter(0, Type::tvoid->pointerTo(), NULL, NULL));
                            params->push(new Parameter(STCin, Type::tsize_t, NULL, NULL));
                            Parameters* dgparams = new Parameters;
                            dgparams->push(new Parameter(0, Type::tvoidptr, NULL, NULL));
                            if (dim == 2)
                                dgparams->push(new Parameter(0, Type::tvoidptr, NULL, NULL));
                            fldeTy[i] = new TypeDelegate(new TypeFunction(dgparams, Type::tint32, 0, LINKd));
                            params->push(new Parameter(0, fldeTy[i], NULL, NULL));
                            fdapply[i] = FuncDeclaration::genCfunc(params, Type::tint32, name[i]);
                        }

                        Expressions *exps = new Expressions();
                        exps->push(fs->aggr);
                        d_uns64 keysize = taa->index->size();
                        if (keysize == SIZE_INVALID)
                            goto Lerror2;
                        assert(keysize < UINT64_MAX - Target::ptrsize);
                        keysize = (keysize + (Target::ptrsize- 1)) & ~(Target::ptrsize - 1);
                        // paint delegate argument to the type runtime expects
                        if (!fldeTy[i]->equals(flde->type))
                        {
                            flde = new CastExp(loc, flde, flde->type);
                            flde->type = fldeTy[i];
                        }
                        exps->push(new IntegerExp(Loc(), keysize, Type::tsize_t));
                        exps->push(flde);

                        ec = new VarExp(Loc(), fdapply[i], false);
                        ec = new CallExp(loc, ec, exps);
                        ec->type = Type::tint32; // don't run semantic() on ec
                    }
                    else if (tab->ty == Tarray || tab->ty == Tsarray)
                    {
                        /* Call:
                         *      _aApply(aggr, flde)
                         */
                        static const char fntab[9][3] =
                        { "cc","cw","cd",
                            "wc","cc","wd",
                            "dc","dw","dd"
                        };
                        const int BUFFER_LEN = 7+1+2+ sizeof(dim)*3 + 1;
                        char fdname[BUFFER_LEN];
                        int flag;

                        switch (tn->ty)
                        {
                            case Tchar:         flag = 0; break;
                            case Twchar:        flag = 3; break;
                            case Tdchar:        flag = 6; break;
                            default:            assert(0);
                        }
                        switch (tnv->ty)
                        {
                            case Tchar:         flag += 0; break;
                            case Twchar:        flag += 1; break;
                            case Tdchar:        flag += 2; break;
                            default:            assert(0);
                        }
                        const char *r = (fs->op == TOKforeach_reverse) ? "R" : "";
                        int j = sprintf(fdname, "_aApply%s%.*s%llu", r, 2, fntab[flag], (ulonglong)dim);
                        assert(j < BUFFER_LEN);

                        FuncDeclaration *fdapply;
                        TypeDelegate *dgty;
                        params = new Parameters();
                        params->push(new Parameter(STCin, tn->arrayOf(), NULL, NULL));
                        Parameters* dgparams = new Parameters;
                        dgparams->push(new Parameter(0, Type::tvoidptr, NULL, NULL));
                        if (dim == 2)
                            dgparams->push(new Parameter(0, Type::tvoidptr, NULL, NULL));
                        dgty = new TypeDelegate(new TypeFunction(dgparams, Type::tint32, 0, LINKd));
                        params->push(new Parameter(0, dgty, NULL, NULL));
                        fdapply = FuncDeclaration::genCfunc(params, Type::tint32, fdname);

                        if (tab->ty == Tsarray)
                            fs->aggr = fs->aggr->castTo(sc2, tn->arrayOf());

                        // paint delegate argument to the type runtime expects
                        if (!dgty->equals(flde->type)) {
                            flde = new CastExp(loc, flde, flde->type);
                            flde->type = dgty;
                        }

                        ec = new VarExp(Loc(), fdapply, false);
                        ec = new CallExp(loc, ec, fs->aggr, flde);
                        ec->type = Type::tint32; // don't run semantic() on ec
                    }
                    else if (tab->ty == Tdelegate)
                    {
                        /* Call:
                         *      aggr(flde)
                         */
                        if (fs->aggr->op == TOKdelegate &&
                            ((DelegateExp *)fs->aggr)->func->isNested())
                        {
                            // See Bugzilla 3560
                            fs->aggr = ((DelegateExp *)fs->aggr)->e1;
                        }
                        ec = new CallExp(loc, fs->aggr, flde);
                        ec = semantic(ec, sc2);
                        if (ec->op == TOKerror)
                            goto Lerror2;
                        if (ec->type != Type::tint32)
                        {
                            fs->error("opApply() function for %s must return an int", tab->toChars());
                            goto Lerror2;
                        }
                    }
                    else
                    {
                        if (global.params.vsafe)
                            fld->tookAddressOf = 1;  // allocate a closure unless the opApply() uses 'scope'

                        assert(tab->ty == Tstruct || tab->ty == Tclass);
                        assert(sapply);
                        /* Call:
                         *  aggr.apply(flde)
                         */
                        ec = new DotIdExp(loc, fs->aggr, sapply->ident);
                        ec = new CallExp(loc, ec, flde);
                        ec = semantic(ec, sc2);
                        if (ec->op == TOKerror)
                            goto Lerror2;
                        if (ec->type != Type::tint32)
                        {
                            fs->error("opApply() function for %s must return an int", tab->toChars());
                            goto Lerror2;
                        }
                    }
                    e = Expression::combine(e, ec);

                    if (!fs->cases->dim)
                    {
                        // Easy case, a clean exit from the loop
                        e = new CastExp(loc, e, Type::tvoid);   // Bugzilla 13899
                        s = new ExpStatement(loc, e);
                    }
                    else
                    {
                        // Construct a switch statement around the return value
                        // of the apply function.
                        Statements *a = new Statements();

                        // default: break; takes care of cases 0 and 1
                        s = new BreakStatement(Loc(), NULL);
                        s = new DefaultStatement(Loc(), s);
                        a->push(s);

                        // cases 2...
                        for (size_t i = 0; i < fs->cases->dim; i++)
                        {
                            s = (*fs->cases)[i];
                            s = new CaseStatement(Loc(), new IntegerExp(i + 2), s);
                            a->push(s);
                        }

                        s = new CompoundStatement(loc, a);
                        s = new SwitchStatement(loc, e, s, false);
                    }
                    s = semantic(s, sc2);
                    break;
                }
            case Terror:
            Lerror2:
                s = new ErrorStatement();
                break;

            default:
                fs->error("foreach: %s is not an aggregate type", fs->aggr->type->toChars());
                goto Lerror2;
        }
        sc2->noctor--;
        sc2->pop();
        result = s;
    }

    void visit(ForeachRangeStatement *fs)
    {
        //printf("ForeachRangeStatement::semantic() %p\n", fs);
        Loc loc = fs->loc;
        fs->lwr = semantic(fs->lwr, sc);
        fs->lwr = resolveProperties(sc, fs->lwr);
        fs->lwr = fs->lwr->optimize(WANTvalue);
        if (!fs->lwr->type)
        {
            fs->error("invalid range lower bound %s", fs->lwr->toChars());
        Lerror:
            return setError();
        }

        fs->upr = semantic(fs->upr, sc);
        fs->upr = resolveProperties(sc, fs->upr);
        fs->upr = fs->upr->optimize(WANTvalue);
        if (!fs->upr->type)
        {
            fs->error("invalid range upper bound %s", fs->upr->toChars());
            goto Lerror;
        }

        if (fs->prm->type)
        {
            fs->prm->type = fs->prm->type->semantic(loc, sc);
            fs->prm->type = fs->prm->type->addStorageClass(fs->prm->storageClass);
            fs->lwr = fs->lwr->implicitCastTo(sc, fs->prm->type);

            if (fs->upr->implicitConvTo(fs->prm->type) || (fs->prm->storageClass & STCref))
            {
                fs->upr = fs->upr->implicitCastTo(sc, fs->prm->type);
            }
            else
            {
                // See if upr-1 fits in prm->type
                Expression *limit = new MinExp(loc, fs->upr, new IntegerExp(1));
                limit = semantic(limit, sc);
                limit = limit->optimize(WANTvalue);
                if (!limit->implicitConvTo(fs->prm->type))
                {
                    fs->upr = fs->upr->implicitCastTo(sc, fs->prm->type);
                }
            }
        }
        else
        {
            /* Must infer types from lwr and upr
            */
            Type *tlwr = fs->lwr->type->toBasetype();
            if (tlwr->ty == Tstruct || tlwr->ty == Tclass)
            {
                /* Just picking the first really isn't good enough.
                */
                fs->prm->type = fs->lwr->type;
            }
            else if (fs->lwr->type == fs->upr->type)
            {
                /* Same logic as CondExp ?lwr:upr
                */
                fs->prm->type = fs->lwr->type;
            }
            else
            {
                AddExp ea(loc, fs->lwr, fs->upr);
                if (typeCombine(&ea, sc))
                    return setError();
                fs->prm->type = ea.type;
                fs->lwr = ea.e1;
                fs->upr = ea.e2;
            }
            fs->prm->type = fs->prm->type->addStorageClass(fs->prm->storageClass);
        }
        if (fs->prm->type->ty == Terror ||
            fs->lwr->op == TOKerror ||
            fs->upr->op == TOKerror)
        {
            return setError();
        }

        /* Convert to a for loop:
         *  foreach (key; lwr .. upr) =>
         *  for (auto key = lwr, auto tmp = upr; key < tmp; ++key)
         *
         *  foreach_reverse (key; lwr .. upr) =>
         *  for (auto tmp = lwr, auto key = upr; key-- > tmp;)
         */
        ExpInitializer *ie = new ExpInitializer(loc, (fs->op == TOKforeach) ? fs->lwr : fs->upr);
        fs->key = new VarDeclaration(loc, fs->upr->type->mutableOf(), Identifier::generateId("__key"), ie);
        fs->key->storage_class |= STCtemp;
        SignExtendedNumber lower = getIntRange(fs->lwr).imin;
        SignExtendedNumber upper = getIntRange(fs->upr).imax;
        if (lower <= upper)
        {
            fs->key->range = new IntRange(lower, upper);
        }

        Identifier *id = Identifier::generateId("__limit");
        ie = new ExpInitializer(loc, (fs->op == TOKforeach) ? fs->upr : fs->lwr);
        VarDeclaration *tmp = new VarDeclaration(loc, fs->upr->type, id, ie);
        tmp->storage_class |= STCtemp;

        Statements *cs = new Statements();
        // Keep order of evaluation as lwr, then upr
        if (fs->op == TOKforeach)
        {
            cs->push(new ExpStatement(loc, fs->key));
            cs->push(new ExpStatement(loc, tmp));
        }
        else
        {
            cs->push(new ExpStatement(loc, tmp));
            cs->push(new ExpStatement(loc, fs->key));
        }
        Statement *forinit = new CompoundDeclarationStatement(loc, cs);

        Expression *cond;
        if (fs->op == TOKforeach_reverse)
        {
            cond = new PostExp(TOKminusminus, loc, new VarExp(loc, fs->key));
            if (fs->prm->type->isscalar())
            {
                // key-- > tmp
                cond = new CmpExp(TOKgt, loc, cond, new VarExp(loc, tmp));
            }
            else
            {
                // key-- != tmp
                cond = new EqualExp(TOKnotequal, loc, cond, new VarExp(loc, tmp));
            }
        }
        else
        {
            if (fs->prm->type->isscalar())
            {
                // key < tmp
                cond = new CmpExp(TOKlt, loc, new VarExp(loc, fs->key), new VarExp(loc, tmp));
            }
            else
            {
                // key != tmp
                cond = new EqualExp(TOKnotequal, loc, new VarExp(loc, fs->key), new VarExp(loc, tmp));
            }
        }

        Expression *increment = NULL;
        if (fs->op == TOKforeach)
        {
            // key += 1
            //increment = new AddAssignExp(loc, new VarExp(loc, key), new IntegerExp(1));
            increment = new PreExp(TOKpreplusplus, loc, new VarExp(loc, fs->key));
        }

        if ((fs->prm->storageClass & STCref) && fs->prm->type->equals(fs->key->type))
        {
            fs->key->range = NULL;
            AliasDeclaration *v = new AliasDeclaration(loc, fs->prm->ident, fs->key);
            fs->_body = new CompoundStatement(loc, new ExpStatement(loc, v), fs->_body);
        }
        else
        {
            ie = new ExpInitializer(loc, new CastExp(loc, new VarExp(loc, fs->key), fs->prm->type));
            VarDeclaration *v = new VarDeclaration(loc, fs->prm->type, fs->prm->ident, ie);
            v->storage_class |= STCtemp | STCforeach | (fs->prm->storageClass & STCref);
            fs->_body = new CompoundStatement(loc, new ExpStatement(loc, v), fs->_body);
            if (fs->key->range && !fs->prm->type->isMutable())
            {
                /* Limit the range of the key to the specified range
                */
                v->range = new IntRange(fs->key->range->imin, fs->key->range->imax - SignExtendedNumber(1));
            }
        }
        if (fs->prm->storageClass & STCref)
        {
            if (fs->key->type->constConv(fs->prm->type) <= MATCHnomatch)
            {
                fs->error("prmument type mismatch, %s to ref %s",
                          fs->key->type->toChars(), fs->prm->type->toChars());
                goto Lerror;
            }
        }

        ForStatement *s = new ForStatement(loc, forinit, cond, increment, fs->_body, fs->endloc);
        if (LabelStatement *ls = checkLabeledLoop(sc, fs))
            ls->gotoTarget = s;
        result = semantic(s, sc);
    }

    void visit(IfStatement *ifs)
    {
        // Evaluate at runtime
        unsigned cs0 = sc->callSuper;
        unsigned cs1;
        unsigned *fi0 = sc->saveFieldInit();
        unsigned *fi1 = NULL;

        // check in syntax level
        ifs->condition = checkAssignmentAsCondition(ifs->condition);

        ScopeDsymbol *sym = new ScopeDsymbol();
        sym->parent = sc->scopesym;
        sym->endlinnum = ifs->endloc.linnum;
        Scope *scd = sc->push(sym);
        if (ifs->prm)
        {
            /* Declare prm, which we will set to be the
             * result of condition.
             */
            ExpInitializer *ei = new ExpInitializer(ifs->loc, ifs->condition);
            ifs->match = new VarDeclaration(ifs->loc, ifs->prm->type, ifs->prm->ident, ei);
            ifs->match->parent = sc->func;
            ifs->match->storage_class |= ifs->prm->storageClass;
            ifs->match->semantic(scd);

            DeclarationExp *de = new DeclarationExp(ifs->loc, ifs->match);
            VarExp *ve = new VarExp(ifs->loc, ifs->match);
            ifs->condition = new CommaExp(ifs->loc, de, ve);
            ifs->condition = semantic(ifs->condition, scd);

            if (ifs->match->edtor)
            {
                Statement *sdtor = new DtorExpStatement(ifs->loc, ifs->match->edtor, ifs->match);
                sdtor = new OnScopeStatement(ifs->loc, TOKon_scope_exit, sdtor);
                ifs->ifbody = new CompoundStatement(ifs->loc, sdtor, ifs->ifbody);
                ifs->match->storage_class |= STCnodtor;
            }
        }
        else
        {
            if (ifs->condition->op == TOKdotid)
                ((DotIdExp *)ifs->condition)->noderef = true;

            ifs->condition = semantic(ifs->condition, sc);
            ifs->condition = resolveProperties(sc, ifs->condition);
            ifs->condition = ifs->condition->addDtorHook(sc);
        }
        ifs->condition = checkGC(sc, ifs->condition);

        // Convert to boolean after declaring prm so this works:
        //  if (S prm = S()) {}
        // where S is a struct that defines opCast!bool.
        ifs->condition = ifs->condition->toBoolean(sc);

        // If we can short-circuit evaluate the if statement, don't do the
        // semantic analysis of the skipped code.
        // This feature allows a limited form of conditional compilation.
        ifs->condition = ifs->condition->optimize(WANTvalue);
        ifs->ifbody = semanticNoScope(ifs->ifbody, scd);
        scd->pop();

        cs1 = sc->callSuper;
        fi1 = sc->fieldinit;
        sc->callSuper = cs0;
        sc->fieldinit = fi0;
        if (ifs->elsebody)
            ifs->elsebody = semanticScope(ifs->elsebody, sc, NULL, NULL);
        sc->mergeCallSuper(ifs->loc, cs1);
        sc->mergeFieldInit(ifs->loc, fi1);

        if (ifs->condition->op == TOKerror ||
            (ifs->ifbody && ifs->ifbody->isErrorStatement()) ||
            (ifs->elsebody && ifs->elsebody->isErrorStatement()))
        {
            return setError();
        }
        result = ifs;
    }

    void visit(ConditionalStatement *cs)
    {
        //printf("ConditionalStatement::semantic()\n");

        // If we can short-circuit evaluate the if statement, don't do the
        // semantic analysis of the skipped code.
        // This feature allows a limited form of conditional compilation.
        if (cs->condition->include(sc, NULL))
        {
            DebugCondition *dc = cs->condition->isDebugCondition();
            if (dc)
            {
                sc = sc->push();
                sc->flags |= SCOPEdebug;
                cs->ifbody = semantic(cs->ifbody, sc);
                sc->pop();
            }
            else
                cs->ifbody = semantic(cs->ifbody, sc);
            result = cs->ifbody;
        }
        else
        {
            if (cs->elsebody)
                cs->elsebody = semantic(cs->elsebody, sc);
            result = cs->elsebody;
        }
    }

    void visit(PragmaStatement *ps)
    {
        // Should be merged with PragmaDeclaration
        //printf("PragmaStatement::semantic() %s\n", ps->toChars());
        //printf("body = %p\n", ps->_body);
        if (ps->ident == Id::msg)
        {
            if (ps->args)
            {
                for (size_t i = 0; i < ps->args->dim; i++)
                {
                    Expression *e = (*ps->args)[i];

                    sc = sc->startCTFE();
                    e = semantic(e, sc);
                    e = resolveProperties(sc, e);
                    sc = sc->endCTFE();
                    // pragma(msg) is allowed to contain types as well as expressions
                    e = ctfeInterpretForPragmaMsg(e);
                    if (e->op == TOKerror)
                    {
                        errorSupplemental(ps->loc, "while evaluating pragma(msg, %s)", (*ps->args)[i]->toChars());
                        goto Lerror;
                    }
                    StringExp *se = e->toStringExp();
                    if (se)
                    {
                        se = se->toUTF8(sc);
                        fprintf(stderr, "%.*s", (int)se->len, (char *)se->string);
                    }
                    else
                        fprintf(stderr, "%s", e->toChars());
                }
                fprintf(stderr, "\n");
            }
        }
        else if (ps->ident == Id::lib)
        {
            /* Should this be allowed?
            */
            ps->error("pragma(lib) not allowed as statement");
            goto Lerror;
        }
        else if (ps->ident == Id::startaddress)
        {
            if (!ps->args || ps->args->dim != 1)
                ps->error("function name expected for start address");
            else
            {
                Expression *e = (*ps->args)[0];

                sc = sc->startCTFE();
                e = semantic(e, sc);
                e = resolveProperties(sc, e);
                sc = sc->endCTFE();

                e = e->ctfeInterpret();
                (*ps->args)[0] = e;
                Dsymbol *sa = getDsymbol(e);
                if (!sa || !sa->isFuncDeclaration())
                {
                    ps->error("function name expected for start address, not '%s'", e->toChars());
                    goto Lerror;
                }
                if (ps->_body)
                {
                    ps->_body = semantic(ps->_body, sc);
                    if (ps->_body->isErrorStatement())
                    {
                        result = ps->_body;
                        return;
                    }
                }
                result = ps;
                return;
            }
        }
        else if (ps->ident == Id::Pinline)
        {
            PINLINE inlining = PINLINEdefault;
            if (!ps->args || ps->args->dim == 0)
                inlining = PINLINEdefault;
            else if (!ps->args || ps->args->dim != 1)
            {
                ps->error("boolean expression expected for pragma(inline)");
                goto Lerror;
            }
            else
            {
                Expression *e = (*ps->args)[0];

                if (e->op != TOKint64 || !e->type->equals(Type::tbool))
                {
                    ps->error("pragma(inline, true or false) expected, not %s", e->toChars());
                    goto Lerror;
                }

                if (e->isBool(true))
                    inlining = PINLINEalways;
                else if (e->isBool(false))
                    inlining = PINLINEnever;

                FuncDeclaration *fd = sc->func;
                if (!fd)
                {
                    ps->error("pragma(inline) is not inside a function");
                    goto Lerror;
                }
                fd->inlining = inlining;
            }
        }
        else
        {
            ps->error("unrecognized pragma(%s)", ps->ident->toChars());
            goto Lerror;
        }

        if (ps->_body)
        {
            ps->_body = semantic(ps->_body, sc);
        }
        result = ps->_body;
        return;

    Lerror:
        return setError();
    }

    void visit(StaticAssertStatement *s)
    {
        s->sa->semantic2(sc);
    }

    void visit(SwitchStatement *ss)
    {
        //printf("SwitchStatement::semantic(%p)\n", ss);
        ss->tf = sc->tf;
        if (ss->cases)
        {
            result = ss;            // already run
            return;
        }
        bool conditionError = false;
        ss->condition = semantic(ss->condition, sc);
        ss->condition = resolveProperties(sc, ss->condition);

        Type *att = NULL;
        TypeEnum *te = NULL;
        while (ss->condition->op != TOKerror)
        {
            // preserve enum type for final switches
            if (ss->condition->type->ty == Tenum)
                te = (TypeEnum *)ss->condition->type;
            if (ss->condition->type->isString())
            {
                // If it's not an array, cast it to one
                if (ss->condition->type->ty != Tarray)
                {
                    ss->condition = ss->condition->implicitCastTo(sc, ss->condition->type->nextOf()->arrayOf());
                }
                ss->condition->type = ss->condition->type->constOf();
                break;
            }
            ss->condition = integralPromotions(ss->condition, sc);
            if (ss->condition->op != TOKerror && ss->condition->type->isintegral())
                break;

            AggregateDeclaration *ad = isAggregate(ss->condition->type);
            if (ad && ad->aliasthis && ss->condition->type != att)
            {
                if (!att && ss->condition->type->checkAliasThisRec())
                    att = ss->condition->type;
                if (Expression *e = resolveAliasThis(sc, ss->condition, true))
                {
                    ss->condition = e;
                    continue;
                }
            }

            if (ss->condition->op != TOKerror)
            {
                ss->error("'%s' must be of integral or string type, it is a %s",
                    ss->condition->toChars(), ss->condition->type->toChars());
                conditionError = true;
                break;
            }
        }
        ss->condition = ss->condition->optimize(WANTvalue);
        ss->condition = checkGC(sc, ss->condition);
        if (ss->condition->op == TOKerror)
            conditionError = true;

        bool needswitcherror = false;

        ss->lastVar = sc->lastVar;

        sc = sc->push();
        sc->sbreak = ss;
        sc->sw = ss;

        ss->cases = new CaseStatements();
        sc->noctor++;       // BUG: should use Scope::mergeCallSuper() for each case instead
        ss->_body = semantic(ss->_body, sc);
        sc->noctor--;

        if (conditionError || ss->_body->isErrorStatement())
            goto Lerror;

        // Resolve any goto case's with exp
        for (size_t i = 0; i < ss->gotoCases.dim; i++)
        {
            GotoCaseStatement *gcs = ss->gotoCases[i];

            if (!gcs->exp)
            {
                gcs->error("no case statement following goto case;");
                goto Lerror;
            }

            for (Scope *scx = sc; scx; scx = scx->enclosing)
            {
                if (!scx->sw)
                    continue;
                for (size_t j = 0; j < scx->sw->cases->dim; j++)
                {
                    CaseStatement *cs = (*scx->sw->cases)[j];

                    if (cs->exp->equals(gcs->exp))
                    {
                        gcs->cs = cs;
                        goto Lfoundcase;
                    }
                }
            }
            gcs->error("case %s not found", gcs->exp->toChars());
            goto Lerror;

        Lfoundcase:
            ;
        }

        if (ss->isFinal)
        {
            Type *t = ss->condition->type;
            Dsymbol *ds;
            EnumDeclaration *ed = NULL;
            if (t && ((ds = t->toDsymbol(sc)) != NULL))
                ed = ds->isEnumDeclaration();  // typedef'ed enum
            if (!ed && te && ((ds = te->toDsymbol(sc)) != NULL))
                ed = ds->isEnumDeclaration();
            if (ed)
            {
                size_t dim = ed->members->dim;
                for (size_t i = 0; i < dim; i++)
                {
                    EnumMember *em = (*ed->members)[i]->isEnumMember();
                    if (em)
                    {
                        for (size_t j = 0; j < ss->cases->dim; j++)
                        {
                            CaseStatement *cs = (*ss->cases)[j];
                            if (cs->exp->equals(em->value()) ||
                                (!cs->exp->type->isString() && !em->value()->type->isString() &&
                                 cs->exp->toInteger() == em->value()->toInteger()))
                                goto L1;
                        }
                        ss->error("enum member %s not represented in final switch", em->toChars());
                        goto Lerror;
                    }
                L1:
                    ;
                }
            }
            else
                needswitcherror = true;
        }

        if (!sc->sw->sdefault && (!ss->isFinal || needswitcherror || global.params.useAssert))
        {
            ss->hasNoDefault = 1;

            if (!ss->isFinal && !ss->_body->isErrorStatement())
                ss->error("switch statement without a default; use 'final switch' or add 'default: assert(0);' or add 'default: break;'");

            // Generate runtime error if the default is hit
            Statements *a = new Statements();
            CompoundStatement *cs;
            Statement *s;

            if (global.params.useSwitchError &&
                global.params.checkAction != CHECKACTION_halt)
            {
                if (global.params.checkAction == CHECKACTION_C)
                {
                    /* Rewrite as an assert(0) and let e2ir generate
                     * the call to the C assert failure function
                     */
                    s = new ExpStatement(ss->loc, new AssertExp(ss->loc, new IntegerExp(ss->loc, 0, Type::tint32)));
                }
                else
                    s = new SwitchErrorStatement(ss->loc);
            }
            else
                s = new ExpStatement(ss->loc, new HaltExp(ss->loc));

            a->reserve(2);
            sc->sw->sdefault = new DefaultStatement(ss->loc, s);
            a->push(ss->_body);
            if (blockExit(ss->_body, sc->func, false) & BEfallthru)
                a->push(new BreakStatement(Loc(), NULL));
            a->push(sc->sw->sdefault);
            cs = new CompoundStatement(ss->loc, a);
            ss->_body = cs;
        }

        if (ss->checkLabel())
            goto Lerror;

        sc->pop();
        result = ss;
        return;

    Lerror:
        sc->pop();
        result = new ErrorStatement();
    }

    void visit(CaseStatement *cs)
    {
        SwitchStatement *sw = sc->sw;
        bool errors = false;

        //printf("CaseStatement::semantic() %s\n", cs->toChars());
        sc = sc->startCTFE();
        cs->exp = semantic(cs->exp, sc);
        cs->exp = resolveProperties(sc, cs->exp);
        sc = sc->endCTFE();
        if (sw)
        {
            cs->exp = cs->exp->implicitCastTo(sc, sw->condition->type);
            cs->exp = cs->exp->optimize(WANTvalue | WANTexpand);

            Expression *e = cs->exp;
            // Remove all the casts the user and/or implicitCastTo may introduce
            // otherwise we'd sometimes fail the check below.
            while (e->op == TOKcast)
                e = ((CastExp *)e)->e1;

            /* This is where variables are allowed as case expressions.
             */
            if (e->op == TOKvar)
            {
                VarExp *ve = (VarExp *)e;
                VarDeclaration *v = ve->var->isVarDeclaration();
                Type *t = cs->exp->type->toBasetype();
                if (v && (t->isintegral() || t->ty == Tclass))
                {
                    /* Flag that we need to do special code generation
                     * for this, i.e. generate a sequence of if-then-else
                     */
                    sw->hasVars = 1;

                    /* TODO check if v can be uninitialized at that point.
                     */
                    if (!v->isConst() && !v->isImmutable())
                    {
                        cs->deprecation("case variables have to be const or immutable");
                    }

                    if (sw->isFinal)
                    {
                        cs->error("case variables not allowed in final switch statements");
                        errors = true;
                    }

                    /* Also check if the VarExp is declared in a scope outside of this one.
                     * 'scx' is set to the scope of the switch statement.
                     */
                    for (Scope *scx = sc; scx; scx = scx->enclosing)
                    {
                        if (scx->enclosing && scx->enclosing->sw == sw)
                            continue;
                        assert(scx->sw == sw);

                        if (!scx->search(cs->exp->loc, v->ident, NULL))
                        {
                            cs->error("case variable `%s` declared at %s cannot be declared in switch body",
                                v->toChars(), v->loc.toChars());
                            errors = true;
                        }
                        break;
                    }
                    goto L1;
                }
            }
            else
                cs->exp = cs->exp->ctfeInterpret();

            if (StringExp *se = cs->exp->toStringExp())
                cs->exp = se;
            else if (cs->exp->op != TOKint64 && cs->exp->op != TOKerror)
            {
                cs->error("case must be a string or an integral constant, not %s", cs->exp->toChars());
                errors = true;
            }

        L1:
            for (size_t i = 0; i < sw->cases->dim; i++)
            {
                CaseStatement *cs2 = (*sw->cases)[i];

                //printf("comparing '%s' with '%s'\n", cs->exp->toChars(), cs2->exp->toChars());
                if (cs2->exp->equals(cs->exp))
                {
                    cs->error("duplicate case %s in switch statement", cs->exp->toChars());
                    errors = true;
                    break;
                }
            }

            sw->cases->push(cs);

            // Resolve any goto case's with no exp to this case statement
            for (size_t i = 0; i < sw->gotoCases.dim; )
            {
                GotoCaseStatement *gcs = sw->gotoCases[i];

                if (!gcs->exp)
                {
                    gcs->cs = cs;
                    sw->gotoCases.remove(i);        // remove from array
                    continue;
                }
                i++;
            }

            if (sc->sw->tf != sc->tf)
            {
                cs->error("switch and case are in different finally blocks");
                errors = true;
            }
        }
        else
        {
            cs->error("case not in switch statement");
            errors = true;
        }
        cs->statement = semantic(cs->statement, sc);
        if (cs->statement->isErrorStatement())
        {
            result = cs->statement;
            return;
        }
        if (errors || cs->exp->op == TOKerror)
            return setError();

        cs->lastVar = sc->lastVar;
        result = cs;
    }

    void visit(CaseRangeStatement *crs)
    {
        SwitchStatement *sw = sc->sw;
        if (sw == NULL)
        {
            crs->error("case range not in switch statement");
            return setError();
        }

        //printf("CaseRangeStatement::semantic() %s\n", toChars());
        bool errors = false;
        if (sw->isFinal)
        {
            crs->error("case ranges not allowed in final switch");
            errors = true;
        }

        sc = sc->startCTFE();
        crs->first = semantic(crs->first, sc);
        crs->first = resolveProperties(sc, crs->first);
        sc = sc->endCTFE();
        crs->first = crs->first->implicitCastTo(sc, sw->condition->type);
        crs->first = crs->first->ctfeInterpret();

        sc = sc->startCTFE();
        crs->last = semantic(crs->last, sc);
        crs->last = resolveProperties(sc, crs->last);
        sc = sc->endCTFE();
        crs->last = crs->last->implicitCastTo(sc, sw->condition->type);
        crs->last = crs->last->ctfeInterpret();

        if (crs->first->op == TOKerror || crs->last->op == TOKerror || errors)
        {
            if (crs->statement)
                semantic(crs->statement, sc);
            return setError();
        }

        uinteger_t fval = crs->first->toInteger();
        uinteger_t lval = crs->last->toInteger();


        if ( (crs->first->type->isunsigned()  &&  fval > lval) ||
             (!crs->first->type->isunsigned()  &&  (sinteger_t)fval > (sinteger_t)lval))
        {
            crs->error("first case %s is greater than last case %s",
                       crs->first->toChars(), crs->last->toChars());
            errors = true;
            lval = fval;
        }

        if (lval - fval > 256)
        {
            crs->error("had %llu cases which is more than 256 cases in case range", lval - fval);
            errors = true;
            lval = fval + 256;
        }

        if (errors)
            return setError();

        /* This works by replacing the CaseRange with an array of Case's.
         *
         * case a: .. case b: s;
         *    =>
         * case a:
         *   [...]
         * case b:
         *   s;
         */

        Statements *statements = new Statements();
        for (uinteger_t i = fval; i != lval + 1; i++)
        {
            Statement *s = crs->statement;
            if (i != lval)                          // if not last case
                s = new ExpStatement(crs->loc, (Expression *)NULL);
            Expression *e = new IntegerExp(crs->loc, i, crs->first->type);
            Statement *cs = new CaseStatement(crs->loc, e, s);
            statements->push(cs);
        }
        Statement *s = new CompoundStatement(crs->loc, statements);
        s = semantic(s, sc);
        result = s;
    }

    void visit(DefaultStatement *ds)
    {
        //printf("DefaultStatement::semantic()\n");
        bool errors = false;
        if (sc->sw)
        {
            if (sc->sw->sdefault)
            {
                ds->error("switch statement already has a default");
                errors = true;
            }
            sc->sw->sdefault = ds;

            if (sc->sw->tf != sc->tf)
            {
                ds->error("switch and default are in different finally blocks");
                errors = true;
            }
            if (sc->sw->isFinal)
            {
                ds->error("default statement not allowed in final switch statement");
                errors = true;
            }
        }
        else
        {
            ds->error("default not in switch statement");
            errors = true;
        }
        ds->statement = semantic(ds->statement, sc);
        if (errors || ds->statement->isErrorStatement())
            return setError();

        ds->lastVar = sc->lastVar;
        result = ds;
    }

    void visit(GotoDefaultStatement *gds)
    {
        gds->sw = sc->sw;
        if (!gds->sw)
        {
            gds->error("goto default not in switch statement");
            return setError();
        }
        if (gds->sw->isFinal)
        {
            gds->error("goto default not allowed in final switch statement");
            return setError();
        }
        result = gds;
    }

    void visit(GotoCaseStatement *gcs)
    {
        if (!sc->sw)
        {
            gcs->error("goto case not in switch statement");
            return setError();
        }

        if (gcs->exp)
        {
            gcs->exp = semantic(gcs->exp, sc);
            gcs->exp = gcs->exp->implicitCastTo(sc, sc->sw->condition->type);
            gcs->exp = gcs->exp->optimize(WANTvalue);
            if (gcs->exp->op == TOKerror)
                return setError();
        }

        sc->sw->gotoCases.push(gcs);
        result = gcs;
    }

    void visit(ReturnStatement *rs)
    {
        //printf("ReturnStatement::semantic() %s\n", toChars());

        FuncDeclaration *fd = sc->parent->isFuncDeclaration();

        if (fd->fes)
            fd = fd->fes->func;             // fd is now function enclosing foreach

        TypeFunction *tf = (TypeFunction *)fd->type;
        assert(tf->ty == Tfunction);

        if (rs->exp && rs->exp->op == TOKvar && ((VarExp *)rs->exp)->var == fd->vresult)
        {
            // return vresult;
            if (sc->fes)
            {
                assert(rs->caseDim == 0);
                sc->fes->cases->push(rs);
                result = new ReturnStatement(Loc(), new IntegerExp(sc->fes->cases->dim + 1));
                return;
            }
            if (fd->returnLabel)
            {
                GotoStatement *gs = new GotoStatement(rs->loc, Id::returnLabel);
                gs->label = fd->returnLabel;
                result = gs;
                return;
            }

            if (!fd->returns)
                fd->returns = new ReturnStatements();
            fd->returns->push(rs);
            result = rs;
            return;
        }

        Type *tret = tf->next;
        Type *tbret = tret ? tret->toBasetype() : NULL;

        bool inferRef = (tf->isref && (fd->storage_class & STCauto));
        Expression *e0 = NULL;

        bool errors = false;
        if (sc->flags & SCOPEcontract)
        {
            rs->error("return statements cannot be in contracts");
            errors = true;
        }
        if (sc->os && sc->os->tok != TOKon_scope_failure)
        {
            rs->error("return statements cannot be in %s bodies", Token::toChars(sc->os->tok));
            errors = true;
        }
        if (sc->tf)
        {
            rs->error("return statements cannot be in finally bodies");
            errors = true;
        }

        if (fd->isCtorDeclaration())
        {
            if (rs->exp)
            {
                rs->error("cannot return expression from constructor");
                errors = true;
            }

            // Constructors implicitly do:
            //      return this;
            rs->exp = new ThisExp(Loc());
            rs->exp->type = tret;
        }
        else if (rs->exp)
        {
            fd->hasReturnExp |= (fd->hasReturnExp & 1 ? 16 : 1);

            FuncLiteralDeclaration *fld = fd->isFuncLiteralDeclaration();
            if (tret)
                rs->exp = inferType(rs->exp, tret);
            else if (fld && fld->treq)
                rs->exp = inferType(rs->exp, fld->treq->nextOf()->nextOf());
            rs->exp = semantic(rs->exp, sc);

            // for static alias this: https://issues.dlang.org/show_bug.cgi?id=17684
            if (rs->exp->op == TOKtype)
                rs->exp = resolveAliasThis(sc, rs->exp);

            rs->exp = resolveProperties(sc, rs->exp);
            if (rs->exp->checkType())
                rs->exp = new ErrorExp();
            if (FuncDeclaration *f = isFuncAddress(rs->exp))
            {
                if (fd->inferRetType && f->checkForwardRef(rs->exp->loc))
                    rs->exp = new ErrorExp();
            }
            if (checkNonAssignmentArrayOp(rs->exp))
                rs->exp = new ErrorExp();

            // Extract side-effect part
            rs->exp = Expression::extractLast(rs->exp, &e0);
            if (rs->exp->op == TOKcall)
                rs->exp = valueNoDtor(rs->exp);

            if (e0)
                e0 = e0->optimize(WANTvalue);

            /* Void-return function can have void typed expression
             * on return statement.
             */
            if ((tbret && tbret->ty == Tvoid) || rs->exp->type->ty == Tvoid)
            {
                if (rs->exp->type->ty != Tvoid)
                {
                    rs->error("cannot return non-void from void function");
                    errors = true;

                    rs->exp = new CastExp(rs->loc, rs->exp, Type::tvoid);
                    rs->exp = semantic(rs->exp, sc);
                }

                /* Replace:
                 *      return exp;
                 * with:
                 *      exp; return;
                 */
                e0 = Expression::combine(e0, rs->exp);
                rs->exp = NULL;
            }
            if (e0)
                e0 = checkGC(sc, e0);
        }

        if (rs->exp)
        {
            if (fd->inferRetType)       // infer return type
            {
                if (!tret)
                {
                    tf->next = rs->exp->type;
                }
                else if (tret->ty != Terror && !rs->exp->type->equals(tret))
                {
                    int m1 = rs->exp->type->implicitConvTo(tret);
                    int m2 = tret->implicitConvTo(rs->exp->type);
                    //printf("exp->type = %s m2<-->m1 tret %s\n", rs->exp->type->toChars(), tret->toChars());
                    //printf("m1 = %d, m2 = %d\n", m1, m2);

                    if (m1 && m2)
                        ;
                    else if (!m1 && m2)
                        tf->next = rs->exp->type;
                    else if (m1 && !m2)
                        ;
                    else if (rs->exp->op != TOKerror)
                    {
                        rs->error("mismatched function return type inference of %s and %s",
                                  rs->exp->type->toChars(), tret->toChars());
                        errors = true;
                        tf->next = Type::terror;
                    }
                }

                tret = tf->next;
                tbret = tret->toBasetype();
            }

            if (inferRef)               // deduce 'auto ref'
            {
                /* Determine "refness" of function return:
                 * if it's an lvalue, return by ref, else return by value
                 */
                if (rs->exp->isLvalue())
                {
                    /* May return by ref
                    */
                    if (checkReturnEscapeRef(sc, rs->exp, true))
                        tf->isref = false;  // return by value
                }
                else
                    tf->isref = false;      // return by value

                /* The "refness" is determined by all of return statements.
                 * This means:
                 *    return 3; return x;  // ok, x can be a value
                 *    return x; return 3;  // ok, x can be a value
                 */
            }

            // handle NRVO
            if (fd->nrvo_can && rs->exp->op == TOKvar)
            {
                VarExp *ve = (VarExp *)rs->exp;
                VarDeclaration *v = ve->var->isVarDeclaration();

                if (tf->isref)
                {
                    // Function returns a reference
                    if (!inferRef)
                        fd->nrvo_can = 0;
                }
                else if (!v || v->isOut() || v->isRef())
                    fd->nrvo_can = 0;
                else if (fd->nrvo_var == NULL)
                {
                    if (!v->isDataseg() && !v->isParameter() && v->toParent2() == fd)
                    {
                        //printf("Setting nrvo to %s\n", v->toChars());
                        fd->nrvo_var = v;
                    }
                    else
                        fd->nrvo_can = 0;
                }
                else if (fd->nrvo_var != v)
                    fd->nrvo_can = 0;
            }
            else //if (!exp->isLvalue())    // keep NRVO-ability
                fd->nrvo_can = 0;
        }
        else
        {
            // handle NRVO
            fd->nrvo_can = 0;

            // infer return type
            if (fd->inferRetType)
            {
                if (tf->next && tf->next->ty != Tvoid)
                {
                    if (tf->next->ty != Terror)
                    {
                        rs->error("mismatched function return type inference of void and %s",
                                  tf->next->toChars());
                    }
                    errors = true;
                    tf->next = Type::terror;
                }
                else
                    tf->next = Type::tvoid;

                tret = tf->next;
                tbret = tret->toBasetype();
            }

            if (inferRef)               // deduce 'auto ref'
                tf->isref = false;

            if (tbret->ty != Tvoid)     // if non-void return
            {
                if (tbret->ty != Terror)
                    rs->error("return expression expected");
                errors = true;
            }
            else if (fd->isMain())
            {
                // main() returns 0, even if it returns void
                rs->exp = new IntegerExp(0);
            }
        }

        // If any branches have called a ctor, but this branch hasn't, it's an error
        if (sc->callSuper & CSXany_ctor &&
            !(sc->callSuper & (CSXthis_ctor | CSXsuper_ctor)))
        {
            rs->error("return without calling constructor");
            errors = true;
        }
        sc->callSuper |= CSXreturn;
        if (sc->fieldinit)
        {
            AggregateDeclaration *ad = fd->isMember2();
            assert(ad);
            size_t dim = sc->fieldinit_dim;
            for (size_t i = 0; i < dim; i++)
            {
                VarDeclaration *v = ad->fields[i];
                bool mustInit = (v->storage_class & STCnodefaultctor ||
                                 v->type->needsNested());
                if (mustInit && !(sc->fieldinit[i] & CSXthis_ctor))
                {
                    rs->error("an earlier return statement skips field %s initialization", v->toChars());
                    errors = true;
                }
                sc->fieldinit[i] |= CSXreturn;
            }
        }

        if (errors)
            return setError();

        if (sc->fes)
        {
            if (!rs->exp)
            {
                // Send out "case receiver" statement to the foreach.
                //  return exp;
                Statement *s = new ReturnStatement(Loc(), rs->exp);
                sc->fes->cases->push(s);

                // Immediately rewrite "this" return statement as:
                //  return cases->dim+1;
                rs->exp = new IntegerExp(sc->fes->cases->dim + 1);
                if (e0)
                {
                    result = new CompoundStatement(rs->loc, new ExpStatement(rs->loc, e0), rs);
                    return;
                }
                result = rs;
                return;
            }
            else
            {
                fd->buildResultVar(NULL, rs->exp->type);
                bool r = fd->vresult->checkNestedReference(sc, Loc());
                assert(!r);  // vresult should be always accessible

                // Send out "case receiver" statement to the foreach.
                //  return vresult;
                Statement *s = new ReturnStatement(Loc(), new VarExp(Loc(), fd->vresult));
                sc->fes->cases->push(s);

                // Save receiver index for the later rewriting from:
                //  return exp;
                // to:
                //  vresult = exp; retrun caseDim;
                rs->caseDim = sc->fes->cases->dim + 1;
            }
        }
        if (rs->exp)
        {
            if (!fd->returns)
                fd->returns = new ReturnStatements();
            fd->returns->push(rs);
        }
        if (e0)
        {
            result = new CompoundStatement(rs->loc, new ExpStatement(rs->loc, e0), rs);
            return;
        }
        result = rs;
    }

    void visit(BreakStatement *bs)
    {
        //printf("BreakStatement::semantic()\n");
        // If:
        //  break Identifier;
        if (bs->ident)
        {
            bs->ident = fixupLabelName(sc, bs->ident);

            FuncDeclaration *thisfunc = sc->func;

            for (Scope *scx = sc; scx; scx = scx->enclosing)
            {
                if (scx->func != thisfunc)  // if in enclosing function
                {
                    if (sc->fes)            // if this is the body of a foreach
                    {
                        /* Post this statement to the fes, and replace
                         * it with a return value that caller will put into
                         * a switch. Caller will figure out where the break
                         * label actually is.
                         * Case numbers start with 2, not 0, as 0 is continue
                         * and 1 is break.
                         */
                        sc->fes->cases->push(bs);
                        result = new ReturnStatement(Loc(), new IntegerExp(sc->fes->cases->dim + 1));
                        return;
                    }
                    break;                  // can't break to it
                }

                LabelStatement *ls = scx->slabel;
                if (ls && ls->ident == bs->ident)
                {
                    Statement *s = ls->statement;

                    if (!s || !s->hasBreak())
                        bs->error("label '%s' has no break", bs->ident->toChars());
                    else if (ls->tf != sc->tf)
                        bs->error("cannot break out of finally block");
                    else
                    {
                        ls->breaks = true;
                        result = bs;
                        return;
                    }
                    return setError();
                }
            }
            bs->error("enclosing label '%s' for break not found", bs->ident->toChars());
            return setError();
        }
        else if (!sc->sbreak)
        {
            if (sc->os && sc->os->tok != TOKon_scope_failure)
            {
                bs->error("break is not inside %s bodies", Token::toChars(sc->os->tok));
            }
            else if (sc->fes)
            {
                // Replace break; with return 1;
                result = new ReturnStatement(Loc(), new IntegerExp(1));
                return;
            }
            else
                bs->error("break is not inside a loop or switch");
            return setError();
        }
        result = bs;
    }

    void visit(ContinueStatement *cs)
    {
        //printf("ContinueStatement::semantic() %p\n", cs);
        if (cs->ident)
        {
            cs->ident = fixupLabelName(sc, cs->ident);

            Scope *scx;
            FuncDeclaration *thisfunc = sc->func;

            for (scx = sc; scx; scx = scx->enclosing)
            {
                LabelStatement *ls;

                if (scx->func != thisfunc)  // if in enclosing function
                {
                    if (sc->fes)            // if this is the body of a foreach
                    {
                        for (; scx; scx = scx->enclosing)
                        {
                            ls = scx->slabel;
                            if (ls && ls->ident == cs->ident && ls->statement == sc->fes)
                            {
                                // Replace continue ident; with return 0;
                                result = new ReturnStatement(Loc(), new IntegerExp(0));
                                return;
                            }
                        }

                        /* Post this statement to the fes, and replace
                         * it with a return value that caller will put into
                         * a switch. Caller will figure out where the break
                         * label actually is.
                         * Case numbers start with 2, not 0, as 0 is continue
                         * and 1 is break.
                         */
                        sc->fes->cases->push(cs);
                        result = new ReturnStatement(Loc(), new IntegerExp(sc->fes->cases->dim + 1));
                        return;
                    }
                    break;                  // can't continue to it
                }

                ls = scx->slabel;
                if (ls && ls->ident == cs->ident)
                {
                    Statement *s = ls->statement;

                    if (!s || !s->hasContinue())
                        cs->error("label '%s' has no continue", cs->ident->toChars());
                    else if (ls->tf != sc->tf)
                        cs->error("cannot continue out of finally block");
                    else
                    {
                        result = cs;
                        return;
                    }
                    return setError();
                }
            }
            cs->error("enclosing label '%s' for continue not found", cs->ident->toChars());
            return setError();
        }
        else if (!sc->scontinue)
        {
            if (sc->os && sc->os->tok != TOKon_scope_failure)
            {
                cs->error("continue is not inside %s bodies", Token::toChars(sc->os->tok));
            }
            else if (sc->fes)
            {
                // Replace continue; with return 0;
                result = new ReturnStatement(Loc(), new IntegerExp(0));
                return;
            }
            else
                cs->error("continue is not inside a loop");
            return setError();
        }
        result = cs;
    }

    void visit(SynchronizedStatement *ss)
    {
        if (ss->exp)
        {
            ss->exp = semantic(ss->exp, sc);
            ss->exp = resolveProperties(sc, ss->exp);
            ss->exp = ss->exp->optimize(WANTvalue);
            ss->exp = checkGC(sc, ss->exp);
            if (ss->exp->op == TOKerror)
                goto Lbody;
            ClassDeclaration *cd = ss->exp->type->isClassHandle();
            if (!cd)
            {
                ss->error("can only synchronize on class objects, not '%s'", ss->exp->type->toChars());
                return setError();
            }
            else if (cd->isInterfaceDeclaration())
            {
                /* Cast the interface to an object, as the object has the monitor,
                 * not the interface.
                 */
                if (!ClassDeclaration::object)
                {
                    ss->error("missing or corrupt object.d");
                    fatal();
                }

                Type *t = ClassDeclaration::object->type;
                t = t->semantic(Loc(), sc)->toBasetype();
                assert(t->ty == Tclass);

                ss->exp = new CastExp(ss->loc, ss->exp, t);
                ss->exp = semantic(ss->exp, sc);
            }

            /* Rewrite as:
             *  auto tmp = exp;
             *  _d_monitorenter(tmp);
             *  try { body } finally { _d_monitorexit(tmp); }
             */
            VarDeclaration *tmp = copyToTemp(0, "__sync", ss->exp);

            Statements *cs = new Statements();
            cs->push(new ExpStatement(ss->loc, tmp));

            Parameters* args = new Parameters;
            args->push(new Parameter(0, ClassDeclaration::object->type, NULL, NULL));

            FuncDeclaration *fdenter = FuncDeclaration::genCfunc(args, Type::tvoid, Id::monitorenter);
            Expression *e = new CallExp(ss->loc, new VarExp(ss->loc, fdenter, false), new VarExp(ss->loc, tmp));
            e->type = Type::tvoid;                  // do not run semantic on e
            cs->push(new ExpStatement(ss->loc, e));

            FuncDeclaration *fdexit = FuncDeclaration::genCfunc(args, Type::tvoid, Id::monitorexit);
            e = new CallExp(ss->loc, new VarExp(ss->loc, fdexit, false), new VarExp(ss->loc, tmp));
            e->type = Type::tvoid;                  // do not run semantic on e
            Statement *s = new ExpStatement(ss->loc, e);
            s = new TryFinallyStatement(ss->loc, ss->_body, s);
            cs->push(s);

            s = new CompoundStatement(ss->loc, cs);
            result = semantic(s, sc);
            return;
        }
        else
        {
            /* Generate our own critical section, then rewrite as:
             *  __gshared byte[CriticalSection.sizeof] critsec;
             *  _d_criticalenter(critsec.ptr);
             *  try { body } finally { _d_criticalexit(critsec.ptr); }
             */
            Identifier *id = Identifier::generateId("__critsec");
            Type *t = Type::tint8->sarrayOf(Target::ptrsize + Target::critsecsize());
            VarDeclaration *tmp = new VarDeclaration(ss->loc, t, id, NULL);
            tmp->storage_class |= STCtemp | STCgshared | STCstatic;

            Statements *cs = new Statements();
            cs->push(new ExpStatement(ss->loc, tmp));

            /* This is just a dummy variable for "goto skips declaration" error.
             * Backend optimizer could remove this unused variable.
             */
            VarDeclaration *v = new VarDeclaration(ss->loc, Type::tvoidptr, Identifier::generateId("__sync"), NULL);
            v->semantic(sc);
            cs->push(new ExpStatement(ss->loc, v));

            Parameters* args = new Parameters;
            args->push(new Parameter(0, t->pointerTo(), NULL, NULL));

            FuncDeclaration *fdenter = FuncDeclaration::genCfunc(args, Type::tvoid, Id::criticalenter, STCnothrow);
            Expression *e = new DotIdExp(ss->loc, new VarExp(ss->loc, tmp), Id::ptr);
            e = semantic(e, sc);
            e = new CallExp(ss->loc, new VarExp(ss->loc, fdenter, false), e);
            e->type = Type::tvoid;                  // do not run semantic on e
            cs->push(new ExpStatement(ss->loc, e));

            FuncDeclaration *fdexit = FuncDeclaration::genCfunc(args, Type::tvoid, Id::criticalexit, STCnothrow);
            e = new DotIdExp(ss->loc, new VarExp(ss->loc, tmp), Id::ptr);
            e = semantic(e, sc);
            e = new CallExp(ss->loc, new VarExp(ss->loc, fdexit, false), e);
            e->type = Type::tvoid;                  // do not run semantic on e
            Statement *s = new ExpStatement(ss->loc, e);
            s = new TryFinallyStatement(ss->loc, ss->_body, s);
            cs->push(s);

            s = new CompoundStatement(ss->loc, cs);
            result = semantic(s, sc);
            return;
        }
    Lbody:
        if (ss->_body)
            ss->_body = semantic(ss->_body, sc);
        if (ss->_body && ss->_body->isErrorStatement())
        {
            result = ss->_body;
            return;
        }
        result = ss;
    }

    void visit(WithStatement *ws)
    {
        ScopeDsymbol *sym;
        Initializer *init;

        //printf("WithStatement::semantic()\n");
        ws->exp = semantic(ws->exp, sc);
        ws->exp = resolveProperties(sc, ws->exp);
        ws->exp = ws->exp->optimize(WANTvalue);
        ws->exp = checkGC(sc, ws->exp);
        if (ws->exp->op == TOKerror)
            return setError();
        if (ws->exp->op == TOKscope)
        {
            sym = new WithScopeSymbol(ws);
            sym->parent = sc->scopesym;
            sym->endlinnum = ws->endloc.linnum;
        }
        else if (ws->exp->op == TOKtype)
        {
            Dsymbol *s = ((TypeExp *)ws->exp)->type->toDsymbol(sc);
            if (!s || !s->isScopeDsymbol())
            {
                ws->error("with type %s has no members", ws->exp->toChars());
                return setError();
            }
            sym = new WithScopeSymbol(ws);
            sym->parent = sc->scopesym;
            sym->endlinnum = ws->endloc.linnum;
        }
        else
        {
            Type *t = ws->exp->type->toBasetype();

            Expression *olde = ws->exp;
            if (t->ty == Tpointer)
            {
                ws->exp = new PtrExp(ws->loc, ws->exp);
                ws->exp = semantic(ws->exp, sc);
                t = ws->exp->type->toBasetype();
            }

            assert(t);
            t = t->toBasetype();
            if (t->isClassHandle())
            {
                init = new ExpInitializer(ws->loc, ws->exp);
                ws->wthis = new VarDeclaration(ws->loc, ws->exp->type, Id::withSym, init);
                ws->wthis->semantic(sc);

                sym = new WithScopeSymbol(ws);
                sym->parent = sc->scopesym;
                sym->endlinnum = ws->endloc.linnum;
            }
            else if (t->ty == Tstruct)
            {
                if (!ws->exp->isLvalue())
                {
                    /* Re-write to
                     * {
                     *   auto __withtmp = exp
                     *   with(__withtmp)
                     *   {
                     *     ...
                     *   }
                     * }
                     */
                    VarDeclaration *tmp = copyToTemp(0, "__withtmp", ws->exp);
                    ExpStatement *es = new ExpStatement(ws->loc, tmp);
                    ws->exp = new VarExp(ws->loc, tmp);
                    Statement *ss = new ScopeStatement(ws->loc, new CompoundStatement(ws->loc, es, ws), ws->endloc);
                    result = semantic(ss, sc);
                    return;
                }
                Expression *e = ws->exp->addressOf();
                init = new ExpInitializer(ws->loc, e);
                ws->wthis = new VarDeclaration(ws->loc, e->type, Id::withSym, init);
                ws->wthis->semantic(sc);
                sym = new WithScopeSymbol(ws);
                // Need to set the scope to make use of resolveAliasThis
                sym->setScope(sc);
                sym->parent = sc->scopesym;
                sym->endlinnum = ws->endloc.linnum;
            }
            else
            {
                ws->error("with expressions must be aggregate types or pointers to them, not '%s'", olde->type->toChars());
                return setError();
            }
        }

        if (ws->_body)
        {
            sym->_scope = sc;
            sc = sc->push(sym);
            sc->insert(sym);
            ws->_body = semantic(ws->_body, sc);
            sc->pop();
            if (ws->_body && ws->_body->isErrorStatement())
            {
                result = ws->_body;
                return;
            }
        }

        result = ws;
    }

    void visit(TryCatchStatement *tcs)
    {
        if (!global.params.useExceptions)
        {
            tcs->error("Cannot use try-catch statements with -betterC");
            return setError();
        }

        if (!ClassDeclaration::throwable)
        {
            tcs->error("Cannot use try-catch statements because `object.Throwable` was not declared");
            return setError();
        }

        unsigned flags = 0;
        const unsigned FLAGcpp = 1;
        const unsigned FLAGd = 2;

        tcs->_body = semanticScope(tcs->_body, sc, NULL, NULL);
        assert(tcs->_body);

        /* Even if body is empty, still do semantic analysis on catches
        */
        bool catchErrors = false;
        for (size_t i = 0; i < tcs->catches->dim; i++)
        {
            Catch *c = (*tcs->catches)[i];
            semantic(c, sc);
            if (c->errors)
            {
                catchErrors = true;
                continue;
            }
            ClassDeclaration *cd = c->type->toBasetype()->isClassHandle();
            flags |= cd->isCPPclass() ? FLAGcpp : FLAGd;

            // Determine if current catch 'hides' any previous catches
            for (size_t j = 0; j < i; j++)
            {
                Catch *cj = (*tcs->catches)[j];
                const char *si = c->loc.toChars();
                const char *sj = cj->loc.toChars();

                if (c->type->toBasetype()->implicitConvTo(cj->type->toBasetype()))
                {
                    tcs->error("catch at %s hides catch at %s", sj, si);
                    catchErrors = true;
                }
            }
        }

        if (sc->func)
        {
            if (flags == (FLAGcpp | FLAGd))
            {
                tcs->error("cannot mix catching D and C++ exceptions in the same try-catch");
                catchErrors = true;
            }
        }

        if (catchErrors)
            return setError();

        if (tcs->_body->isErrorStatement())
        {
            result = tcs->_body;
            return;
        }

        /* If the try body never throws, we can eliminate any catches
         * of recoverable exceptions.
         */

        if (!(blockExit(tcs->_body, sc->func, false) & BEthrow) && ClassDeclaration::exception)
        {
            for (size_t i = 0; i < tcs->catches->dim; i++)
            {
                Catch *c = (*tcs->catches)[i];

                /* If catch exception type is derived from Exception
                */
                if (c->type->toBasetype()->implicitConvTo(ClassDeclaration::exception->type) &&
                    (!c->handler || !c->handler->comeFrom()))
                {
                    // Remove c from the array of catches
                    tcs->catches->remove(i);
                    --i;
                }
            }
        }

        if (tcs->catches->dim == 0)
        {
            result = tcs->_body->hasCode() ? tcs->_body : NULL;
            return;
        }

        result = tcs;
    }

    void visit(TryFinallyStatement *tfs)
    {
        //printf("TryFinallyStatement::semantic()\n");
        tfs->_body = semantic(tfs->_body, sc);
        sc = sc->push();
        sc->tf = tfs;
        sc->sbreak = NULL;
        sc->scontinue = NULL;       // no break or continue out of finally block
        tfs->finalbody = semanticNoScope(tfs->finalbody, sc);
        sc->pop();

        if (!tfs->_body)
        {
            result = tfs->finalbody;
            return;
        }

        if (!tfs->finalbody)
        {
            result = tfs->_body;
            return;
        }

        int blockexit = blockExit(tfs->_body, sc->func, false);

        // if not worrying about exceptions
        if (!(global.params.useExceptions && ClassDeclaration::throwable))
            blockexit &= ~BEthrow;            // don't worry about paths that otherwise may throw

        // Don't care about paths that halt, either
        if ((blockexit & ~BEhalt) == BEfallthru)
        {
            result = new CompoundStatement(tfs->loc, tfs->_body, tfs->finalbody);
            return;
        }
        result = tfs;
    }

    void visit(OnScopeStatement *oss)
    {
        if (oss->tok != TOKon_scope_exit)
        {
            // scope(success) and scope(failure) are rewritten to try-catch(-finally) statement,
            // so the generated catch block cannot be placed in finally block.
            // See also Catch::semantic.
            if (sc->os && sc->os->tok != TOKon_scope_failure)
            {
                // If enclosing is scope(success) or scope(exit), this will be placed in finally block.
                oss->error("cannot put %s statement inside %s", Token::toChars(oss->tok), Token::toChars(sc->os->tok));
                return setError();
            }
            if (sc->tf)
            {
                oss->error("cannot put %s statement inside finally block", Token::toChars(oss->tok));
                return setError();
            }
        }

        sc = sc->push();
        sc->tf = NULL;
        sc->os = oss;
        if (oss->tok != TOKon_scope_failure)
        {
            // Jump out from scope(failure) block is allowed.
            sc->sbreak = NULL;
            sc->scontinue = NULL;
        }
        oss->statement = semanticNoScope(oss->statement, sc);
        sc->pop();

        if (!oss->statement || oss->statement->isErrorStatement())
        {
            result = oss->statement;
            return;
        }
        result = oss;
    }

    void visit(ThrowStatement *ts)
    {
        //printf("ThrowStatement::semantic()\n");

        if (!global.params.useExceptions)
        {
            ts->error("Cannot use `throw` statements with -betterC");
            return setError();
        }

        if (!ClassDeclaration::throwable)
        {
            ts->error("Cannot use `throw` statements because `object.Throwable` was not declared");
            return setError();
        }

        FuncDeclaration *fd = sc->parent->isFuncDeclaration();
        fd->hasReturnExp |= 2;

        ts->exp = semantic(ts->exp, sc);
        ts->exp = resolveProperties(sc, ts->exp);
        ts->exp = checkGC(sc, ts->exp);
        if (ts->exp->op == TOKerror)
            return setError();

        checkThrowEscape(sc, ts->exp, false);

        ClassDeclaration *cd = ts->exp->type->toBasetype()->isClassHandle();
        if (!cd || ((cd != ClassDeclaration::throwable) && !ClassDeclaration::throwable->isBaseOf(cd, NULL)))
        {
            ts->error("can only throw class objects derived from Throwable, not type %s", ts->exp->type->toChars());
            return setError();
        }

        result = ts;
    }

    void visit(DebugStatement *ds)
    {
        if (ds->statement)
        {
            sc = sc->push();
            sc->flags |= SCOPEdebug;
            ds->statement = semantic(ds->statement, sc);
            sc->pop();
        }
        result = ds->statement;
    }

    void visit(GotoStatement *gs)
    {
        //printf("GotoStatement::semantic()\n");
        FuncDeclaration *fd = sc->func;

        gs->ident = fixupLabelName(sc, gs->ident);
        gs->label = fd->searchLabel(gs->ident);
        gs->tf = sc->tf;
        gs->os = sc->os;
        gs->lastVar = sc->lastVar;

        if (!gs->label->statement && sc->fes)
        {
            /* Either the goto label is forward referenced or it
             * is in the function that the enclosing foreach is in.
             * Can't know yet, so wrap the goto in a scope statement
             * so we can patch it later, and add it to a 'look at this later'
             * list.
             */
            ScopeStatement *ss = new ScopeStatement(gs->loc, gs, gs->loc);
            sc->fes->gotos->push(ss);       // 'look at this later' list
            result = ss;
            return;
        }

        // Add to fwdref list to check later
        if (!gs->label->statement)
        {
            if (!fd->gotos)
                fd->gotos = new GotoStatements();
            fd->gotos->push(gs);
        }
        else if (gs->checkLabel())
            return setError();

        result = gs;
    }

    void visit(LabelStatement *ls)
    {
        //printf("LabelStatement::semantic()\n");
        FuncDeclaration *fd = sc->parent->isFuncDeclaration();

        ls->ident = fixupLabelName(sc, ls->ident);
        ls->tf = sc->tf;
        ls->os = sc->os;
        ls->lastVar = sc->lastVar;

        LabelDsymbol *ls2 = fd->searchLabel(ls->ident);
        if (ls2->statement)
        {
            ls->error("label '%s' already defined", ls2->toChars());
            return setError();
        }
        else
            ls2->statement = ls;

        sc = sc->push();
        sc->scopesym = sc->enclosing->scopesym;
        sc->callSuper |= CSXlabel;
        if (sc->fieldinit)
        {
            size_t dim = sc->fieldinit_dim;
            for (size_t i = 0; i < dim; i++)
                sc->fieldinit[i] |= CSXlabel;
        }
        sc->slabel = ls;
        if (ls->statement)
            ls->statement = semantic(ls->statement, sc);
        sc->pop();

        result = ls;
    }

    void visit(AsmStatement *s)
    {
        result = asmSemantic(s, sc);
    }

    void visit(CompoundAsmStatement *cas)
    {
        // Apply postfix attributes of the asm block to each statement.
        sc = sc->push();
        sc->stc |= cas->stc;

        for (size_t i = 0; i < cas->statements->dim; i++)
        {
            Statement *s = (*cas->statements)[i];
            (*cas->statements)[i] = s ? semantic(s, sc) : NULL;
        }

        assert(sc->func);
        // use setImpure/setGC when the deprecation cycle is over
        PURE purity;
        if (!(cas->stc & STCpure) && (purity = sc->func->isPureBypassingInference()) != PUREimpure && purity != PUREfwdref)
            cas->deprecation("asm statement is assumed to be impure - mark it with 'pure' if it is not");
        if (!(cas->stc & STCnogc) && sc->func->isNogcBypassingInference())
            cas->deprecation("asm statement is assumed to use the GC - mark it with '@nogc' if it does not");
        if (!(cas->stc & (STCtrusted|STCsafe)) && sc->func->setUnsafe())
            cas->error("asm statement is assumed to be @system - mark it with '@trusted' if it is not");

        sc->pop();
        result = cas;
    }

    void visit(ImportStatement *imps)
    {
        for (size_t i = 0; i < imps->imports->dim; i++)
        {
            Import *s = (*imps->imports)[i]->isImport();
            assert(!s->aliasdecls.dim);
            for (size_t j = 0; j < s->names.dim; j++)
            {
                Identifier *name = s->names[j];
                Identifier *alias = s->aliases[j];

                if (!alias)
                    alias = name;

                TypeIdentifier *tname = new TypeIdentifier(s->loc, name);
                AliasDeclaration *ad = new AliasDeclaration(s->loc, alias, tname);
                ad->_import = s;
                s->aliasdecls.push(ad);
            }

            s->semantic(sc);
            Module::addDeferredSemantic2(s);     // Bugzilla 14666
            sc->insert(s);

            for (size_t j = 0; j < s->aliasdecls.dim; j++)
            {
                sc->insert(s->aliasdecls[j]);
            }
        }
        result = imps;
    }
};

Statement *semantic(Statement *s, Scope *sc)
{
    StatementSemanticVisitor v = StatementSemanticVisitor(sc);
    s->accept(&v);
    return v.result;
}

void semantic(Catch *c, Scope *sc)
{
    //printf("Catch::semantic(%s)\n", ident->toChars());

    if (sc->os && sc->os->tok != TOKon_scope_failure)
    {
        // If enclosing is scope(success) or scope(exit), this will be placed in finally block.
        error(c->loc, "cannot put catch statement inside %s", Token::toChars(sc->os->tok));
        c->errors = true;
    }
    if (sc->tf)
    {
        /* This is because the _d_local_unwind() gets the stack munged
         * up on this. The workaround is to place any try-catches into
         * a separate function, and call that.
         * To fix, have the compiler automatically convert the finally
         * body into a nested function.
         */
        error(c->loc, "cannot put catch statement inside finally block");
        c->errors = true;
    }

    ScopeDsymbol *sym = new ScopeDsymbol();
    sym->parent = sc->scopesym;
    sc = sc->push(sym);

    if (!c->type)
    {
        deprecation(c->loc, "catch statement without an exception specification is deprecated; use catch(Throwable) for old behavior");

        // reference .object.Throwable
        c->type = getThrowable();
    }
    c->type = c->type->semantic(c->loc, sc);
    if (c->type == Type::terror)
        c->errors = true;
    else
    {
        ClassDeclaration *cd = c->type->toBasetype()->isClassHandle();
        if (!cd)
        {
            error(c->loc, "can only catch class objects, not '%s'", c->type->toChars());
            c->errors = true;
        }
        else if (cd->isCPPclass())
        {
            if (!Target::cppExceptions)
            {
                error(c->loc, "catching C++ class objects not supported for this target");
                c->errors = true;
            }
            if (sc->func && !sc->intypeof && !c->internalCatch && sc->func->setUnsafe())
            {
                error(c->loc, "cannot catch C++ class objects in @safe code");
                c->errors = true;
            }
        }
        else if (cd != ClassDeclaration::throwable && !ClassDeclaration::throwable->isBaseOf(cd, NULL))
        {
            error(c->loc, "can only catch class objects derived from Throwable, not '%s'", c->type->toChars());
            c->errors = true;
        }
        else if (sc->func && !sc->intypeof && !c->internalCatch &&
                 cd != ClassDeclaration::exception && !ClassDeclaration::exception->isBaseOf(cd, NULL) &&
                 sc->func->setUnsafe())
        {
            error(c->loc, "can only catch class objects derived from Exception in @safe code, not '%s'", c->type->toChars());
            c->errors = true;
        }

        if (c->ident)
        {
            c->var = new VarDeclaration(c->loc, c->type, c->ident, NULL);
            c->var->semantic(sc);
            sc->insert(c->var);
        }
        c->handler = semantic(c->handler, sc);
        if (c->handler && c->handler->isErrorStatement())
            c->errors = true;
    }
    sc->pop();
}

Statement *semanticNoScope(Statement *s, Scope *sc)
{
    //printf("Statement::semanticNoScope() %s\n", toChars());
    if (!s->isCompoundStatement() && !s->isScopeStatement())
    {
        s = new CompoundStatement(s->loc, s); // so scopeCode() gets called
    }
    s = semantic(s, sc);
    return s;
}

// Same as semanticNoScope(), but do create a new scope
Statement *semanticScope(Statement *s, Scope *sc, Statement *sbreak, Statement *scontinue)
{
    ScopeDsymbol *sym = new ScopeDsymbol();
    sym->parent = sc->scopesym;
    Scope *scd = sc->push(sym);
    if (sbreak)
        scd->sbreak = sbreak;
    if (scontinue)
        scd->scontinue = scontinue;
    s = semanticNoScope(s, scd);
    scd->pop();
    return s;
}
