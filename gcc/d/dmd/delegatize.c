
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/delegatize.c
 */

#include "root/dsystem.h"

#include "mars.h"
#include "expression.h"
#include "statement.h"
#include "mtype.h"
#include "utf.h"
#include "declaration.h"
#include "aggregate.h"
#include "scope.h"
#include "init.h"
#include "tokens.h"


bool walkPostorder(Expression *e, StoppableVisitor *v);
void lambdaSetParent(Expression *e, Scope *sc);
bool lambdaCheckForNestedRef(Expression *e, Scope *sc);
Expression *semantic(Expression *e, Scope *sc);

/********************************************
 * Convert from expression to delegate that returns the expression,
 * i.e. convert:
 *      expr
 * to:
 *      typeof(expr) delegate() { return expr; }
 */
Expression *toDelegate(Expression *e, Type* t, Scope *sc)
{
    //printf("Expression::toDelegate(t = %s) %s\n", t->toChars(), e->toChars());
    Loc loc = e->loc;

    TypeFunction *tf = new TypeFunction(ParameterList(), t, LINKd);
    if (t->hasWild())
        tf->mod = MODwild;
    FuncLiteralDeclaration *fld =
        new FuncLiteralDeclaration(loc, loc, tf, TOKdelegate, NULL);

    sc = sc->push();
    sc->parent = fld;           // set current function to be the delegate
    lambdaSetParent(e, sc);
    bool r = lambdaCheckForNestedRef(e, sc);
    sc = sc->pop();

    if (r)
        return new ErrorExp();

    Statement *s;
    if (t->ty == Tvoid)
        s = new ExpStatement(loc, e);
    else
        s = new ReturnStatement(loc, e);
    fld->fbody = s;

    e = new FuncExp(loc, fld);
    e = semantic(e, sc);
    return e;
}

/******************************************
 * Patch the parent of declarations to be the new function literal.
 */
void lambdaSetParent(Expression *e, Scope *sc)
{
    class LambdaSetParent : public StoppableVisitor
    {
        Scope *sc;
    public:
        LambdaSetParent(Scope *sc) : sc(sc) {}

        void visit(Expression *)
        {
        }

        void visit(DeclarationExp *e)
        {
            e->declaration->parent = sc->parent;
        }

        void visit(IndexExp *e)
        {
            if (e->lengthVar)
            {
                //printf("lengthVar\n");
                e->lengthVar->parent = sc->parent;
            }
        }

        void visit(SliceExp *e)
        {
            if (e->lengthVar)
            {
                //printf("lengthVar\n");
                e->lengthVar->parent = sc->parent;
            }
        }
    };

    LambdaSetParent lsp(sc);
    walkPostorder(e, &lsp);
}

/*******************************************
 * Look for references to variables in a scope enclosing the new function literal.
 * Returns true if error occurs.
 */
bool lambdaCheckForNestedRef(Expression *e, Scope *sc)
{
    class LambdaCheckForNestedRef : public StoppableVisitor
    {
    public:
        Scope *sc;
        bool result;

        LambdaCheckForNestedRef(Scope *sc)
            : sc(sc), result(false)
        {
        }

        void visit(Expression *)
        {
        }

        void visit(SymOffExp *e)
        {
            VarDeclaration *v = e->var->isVarDeclaration();
            if (v)
                result = v->checkNestedReference(sc, Loc());
        }

        void visit(VarExp *e)
        {
            VarDeclaration *v = e->var->isVarDeclaration();
            if (v)
                result = v->checkNestedReference(sc, Loc());
        }

        void visit(ThisExp *e)
        {
            if (e->var)
                result = e->var->checkNestedReference(sc, Loc());
        }

        void visit(DeclarationExp *e)
        {
            VarDeclaration *v = e->declaration->isVarDeclaration();
            if (v)
            {
                result = v->checkNestedReference(sc, Loc());
                if (result)
                    return;

                /* Some expressions cause the frontend to create a temporary.
                 * For example, structs with cpctors replace the original
                 * expression e with:
                 *  __cpcttmp = __cpcttmp.cpctor(e);
                 *
                 * In this instance, we need to ensure that the original
                 * expression e does not have any nested references by
                 * checking the declaration initializer too.
                 */
                if (v->_init && v->_init->isExpInitializer())
                {
                    Expression *ie = initializerToExpression(v->_init);
                    result = lambdaCheckForNestedRef(ie, sc);
                }
            }
        }
    };

    LambdaCheckForNestedRef v(sc);
    walkPostorder(e, &v);
    return v.result;
}

bool checkNestedRef(Dsymbol *s, Dsymbol *p)
{
    while (s)
    {
        if (s == p) // hit!
            return false;

        if (FuncDeclaration *fd = s->isFuncDeclaration())
        {
            if (!fd->isThis() && !fd->isNested())
                break;

            // Bugzilla 15332: change to delegate if fd is actually nested.
            if (FuncLiteralDeclaration *fld = fd->isFuncLiteralDeclaration())
                fld->tok = TOKdelegate;
        }
        if (AggregateDeclaration *ad = s->isAggregateDeclaration())
        {
            if (ad->storage_class & STCstatic)
                break;
        }
        s = s->toParent2();
    }
    return true;
}
