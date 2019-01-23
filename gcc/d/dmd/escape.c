
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/escape.c
 */

#include "mars.h"
#include "init.h"
#include "expression.h"
#include "scope.h"
#include "aggregate.h"
#include "declaration.h"
#include "module.h"

/************************************
 * Aggregate the data collected by the escapeBy??() functions.
 */
struct EscapeByResults
{
    VarDeclarations byref;      // array into which variables being returned by ref are inserted
    VarDeclarations byvalue;    // array into which variables with values containing pointers are inserted
    FuncDeclarations byfunc;    // nested functions that are turned into delegates
    Expressions byexp;          // array into which temporaries being returned by ref are inserted
};

static bool checkReturnEscapeImpl(Scope *sc, Expression *e, bool refs, bool gag);
static void inferReturn(FuncDeclaration *fd, VarDeclaration *v);
static void escapeByValue(Expression *e, EscapeByResults *er);
static void escapeByRef(Expression *e, EscapeByResults *er);
static void findAllOuterAccessedVariables(FuncDeclaration *fd, VarDeclarations *vars);

/* 'v' is assigned unsafely to 'par'
*/
static void unsafeAssign(Scope *sc, FuncDeclaration *fdc, Identifier *par, Expression *arg, bool gag,
                         bool &result, VarDeclaration *v, const char *desc)
{
    if (global.params.vsafe && sc->func->setUnsafe())
    {
        if (!gag)
            error(arg->loc, "%s %s assigned to non-scope parameter %s calling %s",
                desc, v->toChars(),
                par ? par->toChars() : "unnamed",
                fdc ? fdc->toPrettyChars() : "indirectly");
        result = true;
    }
}

/****************************************
 * Function parameter par is being initialized to arg,
 * and par may escape.
 * Detect if scoped values can escape this way.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      par = identifier of function parameter
 *      arg = initializer for param
 *      gag = do not print error messages
 * Returns:
 *      true if pointers to the stack can escape via assignment
 */
bool checkParamArgumentEscape(Scope *sc, FuncDeclaration *fdc, Identifier *par, Expression *arg, bool gag)
{
    //printf("checkParamArgumentEscape(arg: %s par: %s)\n", arg->toChars(), par->toChars());
    //printf("type = %s, %d\n", arg->type->toChars(), arg->type->hasPointers());

    if (!arg->type->hasPointers())
        return false;

    EscapeByResults er;

    escapeByValue(arg, &er);

    if (!er.byref.dim && !er.byvalue.dim && !er.byfunc.dim && !er.byexp.dim)
        return false;

    bool result = false;

    for (size_t i = 0; i < er.byvalue.dim; i++)
    {
        //printf("byvalue %s\n", v->toChars());
        VarDeclaration *v = er.byvalue[i];
        if (v->isDataseg())
            continue;

        Dsymbol *p = v->toParent2();

        v->storage_class &= ~STCmaybescope;

        if (v->isScope())
        {
            unsafeAssign(sc, fdc, par, arg, gag, result, v, "scope variable");
        }
        else if (v->storage_class & STCvariadic && p == sc->func)
        {
            Type *tb = v->type->toBasetype();
            if (tb->ty == Tarray || tb->ty == Tsarray)
            {
                unsafeAssign(sc, fdc, par, arg, gag, result, v, "variadic variable");
            }
        }
        else
        {
            /* v is not 'scope', and is assigned to a parameter that may escape.
             * Therefore, v can never be 'scope'.
             */
            v->doNotInferScope = true;
        }
    }

    for (size_t i = 0; i < er.byref.dim; i++)
    {
        VarDeclaration *v = er.byref[i];
        if (v->isDataseg())
            continue;

        Dsymbol *p = v->toParent2();

        v->storage_class &= ~STCmaybescope;

        if ((v->storage_class & (STCref | STCout)) == 0 && p == sc->func)
        {
            unsafeAssign(sc, fdc, par, arg, gag, result, v, "reference to local variable");
            continue;
        }
    }

    for (size_t i = 0; i < er.byfunc.dim; i++)
    {
        FuncDeclaration *fd = er.byfunc[i];
        //printf("fd = %s, %d\n", fd->toChars(), fd->tookAddressOf);
        VarDeclarations vars;
        findAllOuterAccessedVariables(fd, &vars);

        for (size_t j = 0; j < vars.dim; j++)
        {
            VarDeclaration *v = vars[j];
            //printf("v = %s\n", v->toChars());
            assert(!v->isDataseg());     // these are not put in the closureVars[]

            Dsymbol *p = v->toParent2();

            v->storage_class &= ~STCmaybescope;

            if ((v->storage_class & (STCref | STCout | STCscope)) && p == sc->func)
            {
                unsafeAssign(sc, fdc, par, arg, gag, result, v, "reference to local");
                continue;
            }
        }
    }

    for (size_t i = 0; i < er.byexp.dim; i++)
    {
        Expression *ee = er.byexp[i];
        if (sc->func->setUnsafe())
        {
            if (!gag)
                error(ee->loc, "reference to stack allocated value returned by %s assigned to non-scope parameter %s",
                    ee->toChars(),
                    par ? par->toChars() : "unnamed");
            result = true;
        }
    }

    return result;
}

/****************************************
 * Given an AssignExp, determine if the lvalue will cause
 * the contents of the rvalue to escape.
 * Print error messages when these are detected.
 * Infer 'scope' for the lvalue where possible, in order
 * to eliminate the error.
 * Params:
 *      sc = used to determine current function and module
 *      ae = AssignExp to check for any pointers to the stack
 *      gag = do not print error messages
 * Returns:
 *      true if pointers to the stack can escape via assignment
 */
bool checkAssignEscape(Scope *sc, Expression *e, bool gag)
{
    //printf("checkAssignEscape(e: %s)\n", e->toChars());
    if (e->op != TOKassign && e->op != TOKblit && e->op != TOKconstruct)
        return false;
    AssignExp *ae = (AssignExp *)e;
    Expression *e1 = ae->e1;
    Expression *e2 = ae->e2;
    //printf("type = %s, %d\n", e1->type->toChars(), e1->type->hasPointers());

    if (!e1->type->hasPointers())
        return false;

    if (e1->op == TOKslice)
        return false;

    EscapeByResults er;

    escapeByValue(e2, &er);

    if (!er.byref.dim && !er.byvalue.dim && !er.byfunc.dim && !er.byexp.dim)
        return false;

    VarDeclaration *va = NULL;
    while (e1->op == TOKdotvar)
        e1 = ((DotVarExp *)e1)->e1;

    if (e1->op == TOKvar)
        va = ((VarExp *)e1)->var->isVarDeclaration();
    else if (e1->op == TOKthis)
        va = ((ThisExp *)e1)->var->isVarDeclaration();
    else if (e1->op == TOKindex)
    {
        IndexExp *ie = (IndexExp *)e1;
        if (ie->e1->op == TOKvar && ie->e1->type->toBasetype()->ty == Tsarray)
            va = ((VarExp *)ie->e1)->var->isVarDeclaration();
    }

    // Try to infer 'scope' for va if in a function not marked @system
    bool inferScope = false;
    if (va && sc->func && sc->func->type && sc->func->type->ty == Tfunction)
        inferScope = ((TypeFunction *)sc->func->type)->trust != TRUSTsystem;

    bool result = false;
    for (size_t i = 0; i < er.byvalue.dim; i++)
    {
        VarDeclaration *v = er.byvalue[i];
        //printf("byvalue: %s\n", v->toChars());
        if (v->isDataseg())
            continue;

        Dsymbol *p = v->toParent2();

        if (!(va && va->isScope()))
            v->storage_class &= ~STCmaybescope;

        if (v->isScope())
        {
            if (va && va->isScope() && va->storage_class & STCreturn && !(v->storage_class & STCreturn) &&
                sc->func->setUnsafe())
            {
                if (!gag)
                    error(ae->loc, "scope variable %s assigned to return scope %s", v->toChars(), va->toChars());
                result = true;
                continue;
            }

            // If va's lifetime encloses v's, then error
            if (va &&
                ((va->enclosesLifetimeOf(v) && !(v->storage_class & STCparameter)) ||
                 // va is class reference
                 (ae->e1->op == TOKdotvar && va->type->toBasetype()->ty == Tclass && (va->enclosesLifetimeOf(v) || !va->isScope())) ||
                 va->storage_class & STCref) &&
                sc->func->setUnsafe())
            {
                if (!gag)
                    error(ae->loc, "scope variable %s assigned to %s with longer lifetime", v->toChars(), va->toChars());
                result = true;
                continue;
            }

            if (va && !va->isDataseg() && !va->doNotInferScope)
            {
                if (!va->isScope() && inferScope)
                {   //printf("inferring scope for %s\n", va->toChars());
                    va->storage_class |= STCscope | STCscopeinferred;
                    va->storage_class |= v->storage_class & STCreturn;
                }
                continue;
            }
            if (sc->func->setUnsafe())
            {
                if (!gag)
                    error(ae->loc, "scope variable %s assigned to non-scope %s", v->toChars(), e1->toChars());
                result = true;
            }
        }
        else if (v->storage_class & STCvariadic && p == sc->func)
        {
            Type *tb = v->type->toBasetype();
            if (tb->ty == Tarray || tb->ty == Tsarray)
            {
                if (va && !va->isDataseg() && !va->doNotInferScope)
                {
                    if (!va->isScope() && inferScope)
                    {   //printf("inferring scope for %s\n", va->toChars());
                        va->storage_class |= STCscope | STCscopeinferred;
                    }
                    continue;
                }
                if (sc->func->setUnsafe())
                {
                    if (!gag)
                        error(ae->loc, "variadic variable %s assigned to non-scope %s", v->toChars(), e1->toChars());
                    result = true;
                }
            }
        }
        else
        {
            /* v is not 'scope', and we didn't check the scope of where we assigned it to.
             * It may escape via that assignment, therefore, v can never be 'scope'.
             */
            v->doNotInferScope = true;
        }
    }

    for (size_t i = 0; i < er.byref.dim; i++)
    {
        VarDeclaration *v = er.byref[i];
        //printf("byref: %s\n", v->toChars());
        if (v->isDataseg())
            continue;

        Dsymbol *p = v->toParent2();

        // If va's lifetime encloses v's, then error
        if (va &&
            ((va->enclosesLifetimeOf(v) && !(v->storage_class & STCparameter)) || va->storage_class & STCref) &&
            sc->func->setUnsafe())
        {
            if (!gag)
                error(ae->loc, "address of variable %s assigned to %s with longer lifetime", v->toChars(), va->toChars());
            result = true;
            continue;
        }

        if (!(va && va->isScope()))
            v->storage_class &= ~STCmaybescope;

        if ((v->storage_class & (STCref | STCout)) == 0 && p == sc->func)
        {
            if (va && !va->isDataseg() && !va->doNotInferScope)
            {
                if (!va->isScope() && inferScope)
                {   //printf("inferring scope for %s\n", va->toChars());
                    va->storage_class |= STCscope | STCscopeinferred;
                }
                continue;
            }
            if (sc->func->setUnsafe())
            {
                if (!gag)
                    error(ae->loc, "reference to local variable %s assigned to non-scope %s", v->toChars(), e1->toChars());
                result = true;
            }
            continue;
        }
    }

    for (size_t i = 0; i < er.byfunc.dim; i++)
    {
        FuncDeclaration *fd = er.byfunc[i];
        //printf("fd = %s, %d\n", fd->toChars(), fd->tookAddressOf);
        VarDeclarations vars;
        findAllOuterAccessedVariables(fd, &vars);

        for (size_t j = 0; j < vars.dim; j++)
        {
            VarDeclaration *v = vars[j];
            //printf("v = %s\n", v->toChars());
            assert(!v->isDataseg());     // these are not put in the closureVars[]

            Dsymbol *p = v->toParent2();

            if (!(va && va->isScope()))
                v->storage_class &= ~STCmaybescope;

            if ((v->storage_class & (STCref | STCout | STCscope)) && p == sc->func)
            {
                if (va && !va->isDataseg() && !va->doNotInferScope)
                {
                    /* Don't infer STCscope for va, because then a closure
                     * won't be generated for sc->func.
                     */
                    //if (!va->isScope() && inferScope)
                        //va->storage_class |= STCscope | STCscopeinferred;
                    continue;
                }
                if (sc->func->setUnsafe())
                {
                    if (!gag)
                        error(ae->loc, "reference to local %s assigned to non-scope %s in @safe code", v->toChars(), e1->toChars());
                    result = true;
                }
                continue;
            }
        }
    }

    for (size_t i = 0; i < er.byexp.dim; i++)
    {
        Expression *ee = er.byexp[i];
        if (va && !va->isDataseg() && !va->doNotInferScope)
        {
            if (!va->isScope() && inferScope)
            {   //printf("inferring scope for %s\n", va->toChars());
                va->storage_class |= STCscope | STCscopeinferred;
            }
            continue;
        }
        if (sc->func->setUnsafe())
        {
            if (!gag)
                error(ee->loc, "reference to stack allocated value returned by %s assigned to non-scope %s",
                    ee->toChars(), e1->toChars());
            result = true;
        }
    }

    return result;
}

/************************************
 * Detect cases where pointers to the stack can 'escape' the
 * lifetime of the stack frame when throwing `e`.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check for any pointers to the stack
 *      gag = do not print error messages
 * Returns:
 *      true if pointers to the stack can escape
 */
bool checkThrowEscape(Scope *sc, Expression *e, bool gag)
{
    //printf("[%s] checkThrowEscape, e = %s\n", e->loc->toChars(), e->toChars());
    EscapeByResults er;

    escapeByValue(e, &er);

    if (!er.byref.dim && !er.byvalue.dim && !er.byexp.dim)
        return false;

    bool result = false;
    for (size_t i = 0; i < er.byvalue.dim; i++)
    {
        VarDeclaration *v = er.byvalue[i];
        //printf("byvalue %s\n", v->toChars());
        if (v->isDataseg())
            continue;

        if (v->isScope())
        {
            if (sc->_module && sc->_module->isRoot())
            {
                // Only look for errors if in module listed on command line
                if (global.params.vsafe) // https://issues.dlang.org/show_bug.cgi?id=17029
                {
                    if (!gag)
                        error(e->loc, "scope variable %s may not be thrown", v->toChars());
                    result = true;
                }
                continue;
            }
        }
        else
        {
            //printf("no infer for %s\n", v->toChars());
            v->doNotInferScope = true;
        }
    }
    return result;
}

/************************************
 * Detect cases where pointers to the stack can 'escape' the
 * lifetime of the stack frame by returning 'e' by value.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check for any pointers to the stack
 *      gag = do not print error messages
 * Returns:
 *      true if pointers to the stack can escape
 */

bool checkReturnEscape(Scope *sc, Expression *e, bool gag)
{
    //printf("[%s] checkReturnEscape, e = %s\n", e->loc->toChars(), e->toChars());
    return checkReturnEscapeImpl(sc, e, false, gag);
}

/************************************
 * Detect cases where returning 'e' by ref can result in a reference to the stack
 * being returned.
 * Print error messages when these are detected.
 * Params:
 *      sc = used to determine current function and module
 *      e = expression to check
 *      gag = do not print error messages
 * Returns:
 *      true if references to the stack can escape
 */
bool checkReturnEscapeRef(Scope *sc, Expression *e, bool gag)
{
    //printf("[%s] checkReturnEscapeRef, e = %s\n", e->loc.toChars(), e->toChars());
    //printf("current function %s\n", sc->func->toChars());
    //printf("parent2 function %s\n", sc->func->toParent2()->toChars());

    return checkReturnEscapeImpl(sc, e, true, gag);
}

static void escapingRef(VarDeclaration *v, Expression *e, bool &result, bool gag)
{
    if (!gag)
    {
        const char *msg;
        if (v->storage_class & STCparameter)
            msg = "returning `%s` escapes a reference to parameter `%s`, perhaps annotate with `return`";
        else
            msg = "returning `%s` escapes a reference to local variable `%s`";
        error(e->loc, msg, e->toChars(), v->toChars());
    }
    result = true;
}

static bool checkReturnEscapeImpl(Scope *sc, Expression *e, bool refs, bool gag)
{
    //printf("[%s] checkReturnEscapeImpl, e = %s\n", e->loc->toChars(), e->toChars());
    EscapeByResults er;

    if (refs)
        escapeByRef(e, &er);
    else
        escapeByValue(e, &er);

    if (!er.byref.dim && !er.byvalue.dim && !er.byexp.dim)
        return false;

    bool result = false;
    for (size_t i = 0; i < er.byvalue.dim; i++)
    {
        VarDeclaration *v = er.byvalue[i];
        //printf("byvalue %s\n", v->toChars());
        if (v->isDataseg())
            continue;

        Dsymbol *p = v->toParent2();

        if ((v->isScope() || (v->storage_class & STCmaybescope)) &&
            !(v->storage_class & STCreturn) &&
            v->isParameter() &&
            sc->func->flags & FUNCFLAGreturnInprocess &&
            p == sc->func)
        {
            inferReturn(sc->func, v);        // infer addition of 'return'
            continue;
        }

        if (v->isScope())
        {
            if (v->storage_class & STCreturn)
                continue;

            if (sc->_module && sc->_module->isRoot() &&
                /* This case comes up when the ReturnStatement of a __foreachbody is
                 * checked for escapes by the caller of __foreachbody. Skip it.
                 *
                 * struct S { static int opApply(int delegate(S*) dg); }
                 * S* foo() {
                 *    foreach (S* s; S) // create __foreachbody for body of foreach
                 *        return s;     // s is inferred as 'scope' but incorrectly tested in foo()
                 *    return null; }
                 */
                !(!refs && p->parent == sc->func))
            {
                // Only look for errors if in module listed on command line
                if (global.params.vsafe) // https://issues.dlang.org/show_bug.cgi?id=17029
                {
                    if (!gag)
                        error(e->loc, "scope variable %s may not be returned", v->toChars());
                    result = true;
                }
                continue;
            }
        }
        else if (v->storage_class & STCvariadic && p == sc->func)
        {
            Type *tb = v->type->toBasetype();
            if (tb->ty == Tarray || tb->ty == Tsarray)
            {
                if (!gag)
                    error(e->loc, "returning `%s` escapes a reference to variadic parameter `%s`", e->toChars(), v->toChars());
                result = false;
            }
        }
        else
        {
            //printf("no infer for %s\n", v->toChars());
            v->doNotInferScope = true;
        }
    }

    for (size_t i = 0; i < er.byref.dim; i++)
    {
        VarDeclaration *v = er.byref[i];
        //printf("byref %s\n", v->toChars());
        if (v->isDataseg())
            continue;

        Dsymbol *p = v->toParent2();

        if ((v->storage_class & (STCref | STCout)) == 0)
        {
            if (p == sc->func)
            {
                escapingRef(v, e, result, gag);
                continue;
            }
            FuncDeclaration *fd = p->isFuncDeclaration();
            if (fd && sc->func->flags & FUNCFLAGreturnInprocess)
            {
                /* Code like:
                 *   int x;
                 *   auto dg = () { return &x; }
                 * Making it:
                 *   auto dg = () return { return &x; }
                 * Because dg.ptr points to x, this is returning dt.ptr+offset
                 */
                if (global.params.vsafe)
                    sc->func->storage_class |= STCreturn;
            }
        }

        /* Check for returning a ref variable by 'ref', but should be 'return ref'
         * Infer the addition of 'return', or set result to be the offending expression.
         */
        if ( (v->storage_class & (STCref | STCout)) &&
             !(v->storage_class & (STCreturn | STCforeach)))
        {
            if ((sc->func->flags & FUNCFLAGreturnInprocess) && p == sc->func)
            {
                inferReturn(sc->func, v);        // infer addition of 'return'
            }
            else if (global.params.useDIP25 &&
                     sc->_module && sc->_module->isRoot())
            {
                // Only look for errors if in module listed on command line

                if (p == sc->func)
                {
                    //printf("escaping reference to local ref variable %s\n", v->toChars());
                    //printf("storage class = x%llx\n", v->storage_class);
                    escapingRef(v, e, result, gag);
                    continue;
                }
                // Don't need to be concerned if v's parent does not return a ref
                FuncDeclaration *fd = p->isFuncDeclaration();
                if (fd && fd->type && fd->type->ty == Tfunction)
                {
                    TypeFunction *tf = (TypeFunction *)fd->type;
                    if (tf->isref)
                    {
                        if (!gag)
                            error(e->loc, "escaping reference to outer local variable %s", v->toChars());
                        result = true;
                        continue;
                    }
                }
            }
        }
    }

    for (size_t i = 0; i < er.byexp.dim; i++)
    {
        Expression *ee = er.byexp[i];
        //printf("byexp %s\n", ee->toChars());
        if (!gag)
            error(ee->loc, "escaping reference to stack allocated value returned by %s", ee->toChars());
        result = true;
    }

    return result;
}


/*************************************
 * Variable v needs to have 'return' inferred for it.
 * Params:
 *      fd = function that v is a parameter to
 *      v = parameter that needs to be STCreturn
 */

static void inferReturn(FuncDeclaration *fd, VarDeclaration *v)
{
    // v is a local in the current function

    //printf("for function '%s' inferring 'return' for variable '%s'\n", fd->toChars(), v->toChars());
    v->storage_class |= STCreturn;

    TypeFunction *tf = (TypeFunction *)fd->type;
    if (v == fd->vthis)
    {
        /* v is the 'this' reference, so mark the function
         */
        fd->storage_class |= STCreturn;
        if (tf->ty == Tfunction)
        {
            //printf("'this' too %p %s\n", tf, sc->func->toChars());
            tf->isreturn = true;
        }
    }
    else
    {
        // Perform 'return' inference on parameter
        if (tf->ty == Tfunction && tf->parameters)
        {
            const size_t dim = Parameter::dim(tf->parameters);
            for (size_t i = 0; i < dim; i++)
            {
                Parameter *p = Parameter::getNth(tf->parameters, i);
                if (p->ident == v->ident)
                {
                    p->storageClass |= STCreturn;
                    break;              // there can be only one
                }
            }
        }
    }
}


/****************************************
 * e is an expression to be returned by value, and that value contains pointers.
 * Walk e to determine which variables are possibly being
 * returned by value, such as:
 *      int* function(int* p) { return p; }
 * If e is a form of &p, determine which variables have content
 * which is being returned as ref, such as:
 *      int* function(int i) { return &i; }
 * Multiple variables can be inserted, because of expressions like this:
 *      int function(bool b, int i, int* p) { return b ? &i : p; }
 *
 * No side effects.
 *
 * Params:
 *      e = expression to be returned by value
 *      er = where to place collected data
 */
static void escapeByValue(Expression *e, EscapeByResults *er)
{
    //printf("[%s] escapeByValue, e: %s\n", e->loc.toChars(), e->toChars());

    class EscapeVisitor : public Visitor
    {
    public:
        EscapeByResults *er;

        EscapeVisitor(EscapeByResults *er)
            : er(er)
        {
        }

        void visit(Expression *)
        {
        }

        void visit(AddrExp *e)
        {
            escapeByRef(e->e1, er);
        }

        void visit(SymOffExp *e)
        {
            VarDeclaration *v = e->var->isVarDeclaration();
            if (v)
                er->byref.push(v);
        }

        void visit(VarExp *e)
        {
            VarDeclaration *v = e->var->isVarDeclaration();
            if (v)
                er->byvalue.push(v);
        }

        void visit(ThisExp *e)
        {
            if (e->var)
                er->byvalue.push(e->var);
        }

        void visit(DotVarExp *e)
        {
            Type *t = e->e1->type->toBasetype();
            if (t->ty == Tstruct)
                e->e1->accept(this);
        }

        void visit(DelegateExp *e)
        {
            Type *t = e->e1->type->toBasetype();
            if (t->ty == Tclass || t->ty == Tpointer)
                escapeByValue(e->e1, er);
            else
                escapeByRef(e->e1, er);
            er->byfunc.push(e->func);
        }

        void visit(FuncExp *e)
        {
            if (e->fd->tok == TOKdelegate)
                er->byfunc.push(e->fd);
        }

        void visit(TupleExp *)
        {
            assert(0); // should have been lowered by now
        }

        void visit(ArrayLiteralExp *e)
        {
            Type *tb = e->type->toBasetype();
            if (tb->ty == Tsarray || tb->ty == Tarray)
            {
                if (e->basis)
                    e->basis->accept(this);
                for (size_t i = 0; i < e->elements->dim; i++)
                {
                    Expression *el = (*e->elements)[i];
                    if (el)
                        el->accept(this);
                }
            }
        }

        void visit(StructLiteralExp *e)
        {
            if (e->elements)
            {
                for (size_t i = 0; i < e->elements->dim; i++)
                {
                    Expression *ex = (*e->elements)[i];
                    if (ex)
                        ex->accept(this);
                }
            }
        }

        void visit(NewExp *e)
        {
            Type *tb = e->newtype->toBasetype();
            if (tb->ty == Tstruct && !e->member && e->arguments)
            {
                for (size_t i = 0; i < e->arguments->dim; i++)
                {
                    Expression *ex = (*e->arguments)[i];
                    if (ex)
                        ex->accept(this);
                }
            }
        }

        void visit(CastExp *e)
        {
            Type *tb = e->type->toBasetype();
            if (tb->ty == Tarray &&
                e->e1->type->toBasetype()->ty == Tsarray)
            {
                escapeByRef(e->e1, er);
            }
            else
                e->e1->accept(this);
        }

        void visit(SliceExp *e)
        {
            if (e->e1->op == TOKvar)
            {
                VarDeclaration *v = ((VarExp *)e->e1)->var->isVarDeclaration();
                Type *tb = e->type->toBasetype();
                if (v)
                {
                    if (tb->ty == Tsarray)
                        return;
                    if (v->storage_class & STCvariadic)
                    {
                        er->byvalue.push(v);
                        return;
                    }
                }
            }
            Type *t1b = e->e1->type->toBasetype();
            if (t1b->ty == Tsarray)
            {
                Type *tb = e->type->toBasetype();
                if (tb->ty != Tsarray)
                    escapeByRef(e->e1, er);
            }
            else
                e->e1->accept(this);
        }

        void visit(BinExp *e)
        {
            Type *tb = e->type->toBasetype();
            if (tb->ty == Tpointer)
            {
                e->e1->accept(this);
                e->e2->accept(this);
            }
        }

        void visit(BinAssignExp *e)
        {
            e->e1->accept(this);
        }

        void visit(AssignExp *e)
        {
            e->e1->accept(this);
        }

        void visit(CommaExp *e)
        {
            e->e2->accept(this);
        }

        void visit(CondExp *e)
        {
            e->e1->accept(this);
            e->e2->accept(this);
        }

        void visit(CallExp *e)
        {
            //printf("CallExp(): %s\n", e->toChars());
            /* Check each argument that is
             * passed as 'return scope'.
             */
            Type *t1 = e->e1->type->toBasetype();
            TypeFunction *tf = NULL;
            TypeDelegate *dg = NULL;
            if (t1->ty == Tdelegate)
            {
                dg = (TypeDelegate *)t1;
                tf = (TypeFunction *)dg->next;
            }
            else if (t1->ty == Tfunction)
                tf = (TypeFunction *)t1;
            else
                return;

            if (e->arguments && e->arguments->dim)
            {
                /* j=1 if _arguments[] is first argument,
                 * skip it because it is not passed by ref
                 */
                size_t j = (tf->linkage == LINKd && tf->varargs == 1);
                for (size_t i = j; i < e->arguments->dim; ++i)
                {
                    Expression *arg = (*e->arguments)[i];
                    size_t nparams = Parameter::dim(tf->parameters);
                    if (i - j < nparams && i >= j)
                    {
                        Parameter *p = Parameter::getNth(tf->parameters, i - j);
                        const StorageClass stc = tf->parameterStorageClass(p);
                        if ((stc & (STCscope)) && (stc & STCreturn))
                            arg->accept(this);
                        else if ((stc & (STCref)) && (stc & STCreturn))
                            escapeByRef(arg, er);
                    }
                }
            }
            // If 'this' is returned, check it too
            if (e->e1->op == TOKdotvar && t1->ty == Tfunction)
            {
                DotVarExp *dve = (DotVarExp *)e->e1;
                FuncDeclaration *fd = dve->var->isFuncDeclaration();
                AggregateDeclaration *ad = NULL;
                if (global.params.vsafe && tf->isreturn && fd && (ad = fd->isThis()) != NULL)
                {
                    if (ad->isClassDeclaration() || tf->isscope)       // this is 'return scope'
                        dve->e1->accept(this);
                    else if (ad->isStructDeclaration()) // this is 'return ref'
                        escapeByRef(dve->e1, er);
                }
                else if (dve->var->storage_class & STCreturn || tf->isreturn)
                {
                    if (dve->var->storage_class & STCscope)
                        dve->e1->accept(this);
                    else if (dve->var->storage_class & STCref)
                        escapeByRef(dve->e1, er);
                }
            }

            /* If returning the result of a delegate call, the .ptr
             * field of the delegate must be checked.
             */
            if (dg)
            {
                if (tf->isreturn)
                    e->e1->accept(this);
            }
        }
    };

    EscapeVisitor v(er);
    e->accept(&v);
}

/****************************************
 * e is an expression to be returned by 'ref'.
 * Walk e to determine which variables are possibly being
 * returned by ref, such as:
 *      ref int function(int i) { return i; }
 * If e is a form of *p, determine which variables have content
 * which is being returned as ref, such as:
 *      ref int function(int* p) { return *p; }
 * Multiple variables can be inserted, because of expressions like this:
 *      ref int function(bool b, int i, int* p) { return b ? i : *p; }
 *
 * No side effects.
 *
 * Params:
 *      e = expression to be returned by 'ref'
 *      er = where to place collected data
 */
static void escapeByRef(Expression *e, EscapeByResults *er)
{
    //printf("[%s] escapeByRef, e: %s\n", e->loc->toChars(), e->toChars());
    class EscapeRefVisitor : public Visitor
    {
    public:
        EscapeByResults *er;

        EscapeRefVisitor(EscapeByResults *er)
            : er(er)
        {
        }

        void visit(Expression *)
        {
        }

        void visit(VarExp *e)
        {
            VarDeclaration *v = e->var->isVarDeclaration();
            if (v)
            {
                if (v->storage_class & STCref && v->storage_class & (STCforeach | STCtemp) && v->_init)
                {
                    /* If compiler generated ref temporary
                     *   (ref v = ex; ex)
                     * look at the initializer instead
                     */
                    if (ExpInitializer *ez = v->_init->isExpInitializer())
                    {
                        assert(ez->exp && ez->exp->op == TOKconstruct);
                        Expression *ex = ((ConstructExp *)ez->exp)->e2;
                        ex->accept(this);
                    }
                }
                else
                    er->byref.push(v);
            }
        }

        void visit(ThisExp *e)
        {
            if (e->var)
                er->byref.push(e->var);
        }

        void visit(PtrExp *e)
        {
            escapeByValue(e->e1, er);
        }

        void visit(IndexExp *e)
        {
            Type *tb = e->e1->type->toBasetype();
            if (e->e1->op == TOKvar)
            {
                VarDeclaration *v = ((VarExp *)e->e1)->var->isVarDeclaration();
                if (tb->ty == Tarray || tb->ty == Tsarray)
                {
                    if (v->storage_class & STCvariadic)
                    {
                        er->byref.push(v);
                        return;
                    }
                }
            }
            if (tb->ty == Tsarray)
            {
                e->e1->accept(this);
            }
            else if (tb->ty == Tarray)
            {
                escapeByValue(e->e1, er);
            }
        }

        void visit(DotVarExp *e)
        {
            Type *t1b = e->e1->type->toBasetype();
            if (t1b->ty == Tclass)
                escapeByValue(e->e1, er);
            else
                e->e1->accept(this);
        }

        void visit(BinAssignExp *e)
        {
            e->e1->accept(this);
        }

        void visit(AssignExp *e)
        {
            e->e1->accept(this);
        }

        void visit(CommaExp *e)
        {
            e->e2->accept(this);
        }

        void visit(CondExp *e)
        {
            e->e1->accept(this);
            e->e2->accept(this);
        }

        void visit(CallExp *e)
        {
            /* If the function returns by ref, check each argument that is
             * passed as 'return ref'.
             */
            Type *t1 = e->e1->type->toBasetype();
            TypeFunction *tf;
            if (t1->ty == Tdelegate)
                tf = (TypeFunction *)((TypeDelegate *)t1)->next;
            else if (t1->ty == Tfunction)
                tf = (TypeFunction *)t1;
            else
                return;
            if (tf->isref)
            {
                if (e->arguments && e->arguments->dim)
                {
                    /* j=1 if _arguments[] is first argument,
                     * skip it because it is not passed by ref
                     */
                    size_t j = (tf->linkage == LINKd && tf->varargs == 1);

                    for (size_t i = j; i < e->arguments->dim; ++i)
                    {
                        Expression *arg = (*e->arguments)[i];
                        size_t nparams = Parameter::dim(tf->parameters);
                        if (i - j < nparams && i >= j)
                        {
                            Parameter *p = Parameter::getNth(tf->parameters, i - j);
                            const StorageClass stc = tf->parameterStorageClass(p);
                            if ((stc & (STCout | STCref)) && (stc & STCreturn))
                                arg->accept(this);
                            else if ((stc & STCscope) && (stc & STCreturn))
                            {
                                if (arg->op == TOKdelegate)
                                {
                                    DelegateExp *de = (DelegateExp *)arg;
                                    if (de->func->isNested())
                                        er->byexp.push(de);
                                }
                                else
                                    escapeByValue(arg, er);
                            }
                        }
                    }
                }

                // If 'this' is returned by ref, check it too
                if (e->e1->op == TOKdotvar && t1->ty == Tfunction)
                {
                    DotVarExp *dve = (DotVarExp *)e->e1;
                    if (dve->var->storage_class & STCreturn || tf->isreturn)
                    {
                        if ((dve->var->storage_class & STCscope) || tf->isscope)
                            escapeByValue(dve->e1, er);
                        else if ((dve->var->storage_class & STCref) || tf->isref)
                            dve->e1->accept(this);
                    }

                }
                // If it's a delegate, check it too
                if (e->e1->op == TOKvar && t1->ty == Tdelegate)
                {
                    escapeByValue(e->e1, er);
                }
            }
            else
                er->byexp.push(e);
        }
    };

    EscapeRefVisitor v(er);
    e->accept(&v);
}

/*************************
 * Find all variables accessed by this delegate that are
 * in functions enclosing it.
 * Params:
 *      fd = function
 *      vars = array to append found variables to
 */
void findAllOuterAccessedVariables(FuncDeclaration *fd, VarDeclarations *vars)
{
    //printf("findAllOuterAccessedVariables(fd: %s)\n", fd.toChars());
    for (Dsymbol *p = fd->parent; p; p = p->parent)
    {
        FuncDeclaration *fdp = p->isFuncDeclaration();
        if (fdp)
        {
            for (size_t i = 0; i < fdp->closureVars.dim; i++)
            {
                VarDeclaration *v = fdp->closureVars[i];
                for (size_t j = 0; j < v->nestedrefs.dim; j++)
                {
                    FuncDeclaration *fdv = v->nestedrefs[j];
                    if (fdv == fd)
                    {
                        //printf("accessed: %s, type %s\n", v->toChars(), v->type->toChars());
                        vars->push(v);
                    }
                }
            }
        }
    }
}
