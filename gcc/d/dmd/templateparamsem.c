
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "template.h"
#include "mtype.h"
#include "scope.h"
#include "visitor.h"

bool reliesOnTident(Type *t, TemplateParameters *tparams = NULL, size_t iStart = 0);

class TemplateParameterSemanticVisitor : public Visitor
{
public:
    Scope *sc;
    TemplateParameters *parameters;
    bool result;

    TemplateParameterSemanticVisitor(Scope *sc, TemplateParameters *parameters)
    {
        this->sc = sc;
        this->parameters = parameters;
        this->result = false;
    }

    void visit(TemplateTypeParameter *ttp)
    {
        //printf("TemplateTypeParameter::semantic('%s')\n", ident->toChars());
        if (ttp->specType && !reliesOnTident(ttp->specType, parameters))
        {
            ttp->specType = typeSemantic(ttp->specType, ttp->loc, sc);
        }
        result = !(ttp->specType && isError(ttp->specType));
    }

    void visit(TemplateValueParameter *tvp)
    {
        tvp->valType = typeSemantic(tvp->valType, tvp->loc, sc);

        result = !isError(tvp->valType);
    }

    void visit(TemplateAliasParameter *tap)
    {
        if (tap->specType && !reliesOnTident(tap->specType, parameters))
        {
            tap->specType = typeSemantic(tap->specType, tap->loc, sc);
        }
        tap->specAlias = aliasParameterSemantic(tap->loc, sc, tap->specAlias, parameters);
        result = !(tap->specType  && isError(tap->specType)) &&
            !(tap->specAlias && isError(tap->specAlias));
    }

    void visit(TemplateTupleParameter *)
    {
        result = true;
    }
};

/************************************************
 * Performs semantic on TemplateParameter AST nodes.
 *
 * Params:
 *      tp = element of `parameters` to be semantically analyzed
 *      sc = context
 *      parameters = array of `TemplateParameters` supplied to the `TemplateDeclaration`
 * Returns:
 *      `true` if no errors
 */
bool tpsemantic(TemplateParameter *tp, Scope *sc, TemplateParameters *parameters)
{
    TemplateParameterSemanticVisitor v(sc, parameters);
    tp->accept(&v);
    return v.result;
}

/***********************************************
 * Support function for performing semantic analysis on `TemplateAliasParameter`.
 *
 * Params:
 *      loc = location (for error messages)
 *      sc = context
 *      o = object to run semantic() on, the `TemplateAliasParameter`s `specAlias` or `defaultAlias`
 *      parameters = array of `TemplateParameters` supplied to the `TemplateDeclaration`
 * Returns:
 *      object resulting from running `semantic` on `o`
 */
RootObject *aliasParameterSemantic(Loc loc, Scope *sc, RootObject *o, TemplateParameters *parameters)
{
    if (o)
    {
        Expression *ea = isExpression(o);
        Type *ta = isType(o);
        if (ta && (!parameters || !reliesOnTident(ta, parameters)))
        {
            Dsymbol *s = ta->toDsymbol(sc);
            if (s)
                o = s;
            else
                o = typeSemantic(ta, loc, sc);
        }
        else if (ea)
        {
            sc = sc->startCTFE();
            ea = expressionSemantic(ea, sc);
            sc = sc->endCTFE();
            o = ea->ctfeInterpret();
        }
    }
    return o;
}
