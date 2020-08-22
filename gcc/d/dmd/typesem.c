
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "mtype.h"
#include "expression.h"
#include "template.h"

Expression *typeToExpression(Type *t);
Expression *typeToExpressionHelper(TypeQualified *t, Expression *e, size_t i = 0);

class TypeToExpressionVisitor : public Visitor
{
public:
    Expression *result;
    Type *itype;

    TypeToExpressionVisitor(Type *itype)
    {
        this->result = NULL;
        this->itype = itype;
    }

    void visit(Type *)
    {
        result = NULL;
    }

    void visit(TypeSArray *t)
    {
        Expression *e = typeToExpression(t->next);
        if (e)
            e = new ArrayExp(t->dim->loc, e, t->dim);
        result = e;
    }

    void visit(TypeAArray *t)
    {
        Expression *e = typeToExpression(t->next);
        if (e)
        {
            Expression *ei = typeToExpression(t->index);
            if (ei)
            {
                result = new ArrayExp(t->loc, e, ei);
                return;
            }
        }
        result = NULL;
    }

    void visit(TypeIdentifier *t)
    {
        result = typeToExpressionHelper(t, new IdentifierExp(t->loc, t->ident));
    }

    void visit(TypeInstance *t)
    {
        result = typeToExpressionHelper(t, new ScopeExp(t->loc, t->tempinst));
    }
};

/* We've mistakenly parsed this as a type.
 * Redo it as an Expression.
 * NULL if cannot.
 */
Expression *typeToExpression(Type *t)
{
    TypeToExpressionVisitor v = TypeToExpressionVisitor(t);
    t->accept(&v);
    return v.result;
}

/* Helper function for `typeToExpression`. Contains common code
 * for TypeQualified derived classes.
 */
Expression *typeToExpressionHelper(TypeQualified *t, Expression *e, size_t i)
{
    //printf("toExpressionHelper(e = %s %s)\n", Token::toChars(e->op), e->toChars());
    for (; i < t->idents.length; i++)
    {
        RootObject *id = t->idents[i];
        //printf("\t[%d] e: '%s', id: '%s'\n", i, e->toChars(), id->toChars());

        switch (id->dyncast())
        {
            case DYNCAST_IDENTIFIER:
            {
                // ... '. ident'
                e = new DotIdExp(e->loc, e, (Identifier *)id);
                break;
            }
            case DYNCAST_DSYMBOL:
            {
                // ... '. name!(tiargs)'
                TemplateInstance *ti = ((Dsymbol *)id)->isTemplateInstance();
                assert(ti);
                e = new DotTemplateInstanceExp(e->loc, e, ti->name, ti->tiargs);
                break;
            }
            case DYNCAST_TYPE:          // Bugzilla 1215
            {
                // ... '[type]'
                e = new ArrayExp(t->loc, e, new TypeExp(t->loc, (Type *)id));
                break;
            }
            case DYNCAST_EXPRESSION:    // Bugzilla 1215
            {
                // ... '[expr]'
                e = new ArrayExp(t->loc, e, (Expression *)id);
                break;
            }
            default:
                assert(0);
        }
    }
    return e;
}
