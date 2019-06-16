
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/template.c
 */

// Handle template implementation

#include "root/dsystem.h"
#include "root/root.h"
#include "root/aav.h"
#include "root/rmem.h"
#include "root/stringtable.h"
#include "root/hash.h"

#include "mangle.h"
#include "mtype.h"
#include "template.h"
#include "init.h"
#include "expression.h"
#include "scope.h"
#include "module.h"
#include "aggregate.h"
#include "declaration.h"
#include "dsymbol.h"
#include "mars.h"
#include "dsymbol.h"
#include "identifier.h"
#include "hdrgen.h"
#include "id.h"
#include "attrib.h"
#include "tokens.h"

#define IDX_NOTFOUND (0x12345678)               // index is not found

Type *rawTypeMerge(Type *t1, Type *t2);
bool MODimplicitConv(MOD modfrom, MOD modto);
MATCH MODmethodConv(MOD modfrom, MOD modto);
MOD MODmerge(MOD mod1, MOD mod2);

static size_t templateParameterLookup(Type *tparam, TemplateParameters *parameters);
static int arrayObjectMatch(Objects *oa1, Objects *oa2);
static unsigned char deduceWildHelper(Type *t, Type **at, Type *tparam);
static MATCH deduceTypeHelper(Type *t, Type **at, Type *tparam);
static bool reliesOnTident(Type *t, TemplateParameters *tparams = NULL, size_t iStart = 0);
Expression *semantic(Expression *e, Scope *sc);
bool evalStaticCondition(Scope *sc, Expression *exp, Expression *e, bool &errors);

/********************************************
 * These functions substitute for dynamic_cast. dynamic_cast does not work
 * on earlier versions of gcc.
 */

Expression *isExpression(RootObject *o)
{
    //return dynamic_cast<Expression *>(o);
    if (!o || o->dyncast() != DYNCAST_EXPRESSION)
        return NULL;
    return (Expression *)o;
}

Dsymbol *isDsymbol(RootObject *o)
{
    //return dynamic_cast<Dsymbol *>(o);
    if (!o || o->dyncast() != DYNCAST_DSYMBOL)
        return NULL;
    return (Dsymbol *)o;
}

Type *isType(RootObject *o)
{
    //return dynamic_cast<Type *>(o);
    if (!o || o->dyncast() != DYNCAST_TYPE)
        return NULL;
    return (Type *)o;
}

Tuple *isTuple(RootObject *o)
{
    //return dynamic_cast<Tuple *>(o);
    if (!o || o->dyncast() != DYNCAST_TUPLE)
        return NULL;
    return (Tuple *)o;
}

Parameter *isParameter(RootObject *o)
{
    //return dynamic_cast<Parameter *>(o);
    if (!o || o->dyncast() != DYNCAST_PARAMETER)
        return NULL;
    return (Parameter *)o;
}

/**************************************
 * Is this Object an error?
 */
bool isError(RootObject *o)
{
    Type *t = isType(o);
    if (t)
        return (t->ty == Terror);
    Expression *e = isExpression(o);
    if (e)
        return (e->op == TOKerror || !e->type || e->type->ty == Terror);
    Tuple *v = isTuple(o);
    if (v)
        return arrayObjectIsError(&v->objects);
    Dsymbol *s = isDsymbol(o);
    assert(s);
    if (s->errors)
        return true;
    return s->parent ? isError(s->parent) : false;
}

/**************************************
 * Are any of the Objects an error?
 */
bool arrayObjectIsError(Objects *args)
{
    for (size_t i = 0; i < args->dim; i++)
    {
        RootObject *o = (*args)[i];
        if (isError(o))
            return true;
    }
    return false;
}

/***********************
 * Try to get arg as a type.
 */

Type *getType(RootObject *o)
{
    Type *t = isType(o);
    if (!t)
    {
        Expression *e = isExpression(o);
        if (e)
            t = e->type;
    }
    return t;
}

Dsymbol *getDsymbol(RootObject *oarg)
{
    //printf("getDsymbol()\n");
    //printf("e %p s %p t %p v %p\n", isExpression(oarg), isDsymbol(oarg), isType(oarg), isTuple(oarg));

    Dsymbol *sa;
    Expression *ea = isExpression(oarg);
    if (ea)
    {
        // Try to convert Expression to symbol
        if (ea->op == TOKvar)
            sa = ((VarExp *)ea)->var;
        else if (ea->op == TOKfunction)
        {
            if (((FuncExp *)ea)->td)
                sa = ((FuncExp *)ea)->td;
            else
                sa = ((FuncExp *)ea)->fd;
        }
        else if (ea->op == TOKtemplate)
            sa = ((TemplateExp *)ea)->td;
        else
            sa = NULL;
    }
    else
    {
        // Try to convert Type to symbol
        Type *ta = isType(oarg);
        if (ta)
            sa = ta->toDsymbol(NULL);
        else
            sa = isDsymbol(oarg);       // if already a symbol
    }
    return sa;
}

/***********************
 * Try to get value from manifest constant
 */

static Expression *getValue(Expression *e)
{
    if (e && e->op == TOKvar)
    {
        VarDeclaration *v = ((VarExp *)e)->var->isVarDeclaration();
        if (v && v->storage_class & STCmanifest)
        {
            e = v->getConstInitializer();
        }
    }
    return e;
}

static Expression *getValue(Dsymbol *&s)
{
    Expression *e = NULL;
    if (s)
    {
        VarDeclaration *v = s->isVarDeclaration();
        if (v && v->storage_class & STCmanifest)
        {
            e = v->getConstInitializer();
        }
    }
    return e;
}

/**********************************
 * Return true if e could be valid only as a template value parameter.
 * Return false if it might be an alias or tuple.
 * (Note that even in this case, it could still turn out to be a value).
 */
bool definitelyValueParameter(Expression *e)
{
    // None of these can be value parameters
    if (e->op == TOKtuple || e->op == TOKscope  ||
        e->op == TOKtype || e->op == TOKdottype ||
        e->op == TOKtemplate ||  e->op == TOKdottd ||
        e->op == TOKfunction || e->op == TOKerror ||
        e->op == TOKthis || e->op == TOKsuper)
        return false;

    if (e->op != TOKdotvar)
        return true;

    /* Template instantiations involving a DotVar expression are difficult.
     * In most cases, they should be treated as a value parameter, and interpreted.
     * But they might also just be a fully qualified name, which should be treated
     * as an alias.
     */

    // x.y.f cannot be a value
    FuncDeclaration *f = ((DotVarExp *)e)->var->isFuncDeclaration();
    if (f)
        return false;

    while (e->op == TOKdotvar)
    {
        e = ((DotVarExp *)e)->e1;
    }
    // this.x.y and super.x.y couldn't possibly be valid values.
    if (e->op == TOKthis || e->op == TOKsuper)
        return false;

    // e.type.x could be an alias
    if (e->op == TOKdottype)
        return false;

    // var.x.y is the only other possible form of alias
    if (e->op != TOKvar)
        return true;

    VarDeclaration *v = ((VarExp *)e)->var->isVarDeclaration();

    // func.x.y is not an alias
    if (!v)
        return true;

    // TODO: Should we force CTFE if it is a global constant?

    return false;
}

static Expression *getExpression(RootObject *o)
{
    Dsymbol *s = isDsymbol(o);
    return s ? getValue(s) : getValue(isExpression(o));
}

/******************************
 * If o1 matches o2, return true.
 * Else, return false.
 */

static bool match(RootObject *o1, RootObject *o2)
{
    //printf("match() o1 = %p %s (%d), o2 = %p %s (%d)\n",
    //       o1, o1->toChars(), o1->dyncast(), o2, o2->toChars(), o2->dyncast());

    /* A proper implementation of the various equals() overrides
     * should make it possible to just do o1->equals(o2), but
     * we'll do that another day.
     */

    /* Manifest constants should be compared by their values,
     * at least in template arguments.
     */

    if (Type *t1 = isType(o1))
    {
        Type *t2 = isType(o2);
        if (!t2)
            goto Lnomatch;

        //printf("\tt1 = %s\n", t1->toChars());
        //printf("\tt2 = %s\n", t2->toChars());
        if (!t1->equals(t2))
            goto Lnomatch;

        goto Lmatch;
    }
    if (Expression *e1 = getExpression(o1))
    {
        Expression *e2 = getExpression(o2);
        if (!e2)
            goto Lnomatch;

        //printf("\te1 = %s %s %s\n", e1->type->toChars(), Token::toChars(e1->op), e1->toChars());
        //printf("\te2 = %s %s %s\n", e2->type->toChars(), Token::toChars(e2->op), e2->toChars());

        // two expressions can be equal although they do not have the same
        // type; that happens when they have the same value. So check type
        // as well as expression equality to ensure templates are properly
        // matched.
        if (!e1->type->equals(e2->type) || !e1->equals(e2))
            goto Lnomatch;

        goto Lmatch;
    }
    if (Dsymbol *s1 = isDsymbol(o1))
    {
        Dsymbol *s2 = isDsymbol(o2);
        if (!s2)
            goto Lnomatch;

        //printf("\ts1 = %s\n", s1->toChars());
        //printf("\ts2 = %s\n", s2->toChars());
        if (!s1->equals(s2))
            goto Lnomatch;
        if (s1->parent != s2->parent && !s1->isFuncDeclaration() && !s2->isFuncDeclaration())
            goto Lnomatch;

        goto Lmatch;
    }
    if (Tuple *u1 = isTuple(o1))
    {
        Tuple *u2 = isTuple(o2);
        if (!u2)
            goto Lnomatch;

        //printf("\tu1 = %s\n", u1->toChars());
        //printf("\tu2 = %s\n", u2->toChars());
        if (!arrayObjectMatch(&u1->objects, &u2->objects))
            goto Lnomatch;

        goto Lmatch;
    }
Lmatch:
    //printf("\t-> match\n");
    return true;

Lnomatch:
    //printf("\t-> nomatch\n");
    return false;
}


/************************************
 * Match an array of them.
 */
int arrayObjectMatch(Objects *oa1, Objects *oa2)
{
    if (oa1 == oa2)
        return 1;
    if (oa1->dim != oa2->dim)
        return 0;
    for (size_t j = 0; j < oa1->dim; j++)
    {
        RootObject *o1 = (*oa1)[j];
        RootObject *o2 = (*oa2)[j];
        if (!match(o1, o2))
        {
            return 0;
        }
    }
    return 1;
}


/************************************
 * Computes hash of expression.
 * Handles all Expression classes and MUST match their equals method,
 * i.e. e1->equals(e2) implies expressionHash(e1) == expressionHash(e2).
 */
static hash_t expressionHash(Expression *e)
{
    switch (e->op)
    {
    case TOKint64:
        return (size_t) ((IntegerExp *)e)->getInteger();

    case TOKfloat64:
        return CTFloat::hash(((RealExp *)e)->value);

    case TOKcomplex80:
    {
        ComplexExp *ce = (ComplexExp *)e;
        return mixHash(CTFloat::hash(ce->toReal()), CTFloat::hash(ce->toImaginary()));
    }

    case TOKidentifier:
        return (size_t)(void *) ((IdentifierExp *)e)->ident;

    case TOKnull:
        return (size_t)(void *) ((NullExp *)e)->type;

    case TOKstring:
    {
        StringExp *se = (StringExp *)e;
        return calcHash((const char *)se->string, se->len * se->sz);
    }

    case TOKtuple:
    {
        TupleExp *te = (TupleExp *)e;
        size_t hash = 0;
        hash += te->e0 ? expressionHash(te->e0) : 0;
        for (size_t i = 0; i < te->exps->dim; i++)
        {
            Expression *elem = (*te->exps)[i];
            hash = mixHash(hash, expressionHash(elem));
        }
        return hash;
    }

    case TOKarrayliteral:
    {
        ArrayLiteralExp *ae = (ArrayLiteralExp *)e;
        size_t hash = 0;
        for (size_t i = 0; i < ae->elements->dim; i++)
            hash = mixHash(hash, expressionHash(ae->getElement(i)));
        return hash;
    }

    case TOKassocarrayliteral:
    {
        AssocArrayLiteralExp *ae = (AssocArrayLiteralExp *)e;
        size_t hash = 0;
        for (size_t i = 0; i < ae->keys->dim; i++)
            // reduction needs associative op as keys are unsorted (use XOR)
            hash ^= mixHash(expressionHash((*ae->keys)[i]), expressionHash((*ae->values)[i]));
        return hash;
    }

    case TOKstructliteral:
    {
        StructLiteralExp *se = (StructLiteralExp *)e;
        size_t hash = 0;
        for (size_t i = 0; i < se->elements->dim; i++)
        {
            Expression *elem = (*se->elements)[i];
            hash = mixHash(hash, elem ? expressionHash(elem) : 0);
        }
        return hash;
    }

    case TOKvar:
        return (size_t)(void *) ((VarExp *)e)->var;

    case TOKfunction:
        return (size_t)(void *) ((FuncExp *)e)->fd;

    default:
        // no custom equals for this expression
        // equals based on identity
        return (size_t)(void *) e;
    }
}


/************************************
 * Return hash of Objects.
 */
static hash_t arrayObjectHash(Objects *oa1)
{
    hash_t hash = 0;
    for (size_t j = 0; j < oa1->dim; j++)
    {
        /* Must follow the logic of match()
         */
        RootObject *o1 = (*oa1)[j];
        if (Type *t1 = isType(o1))
            hash = mixHash(hash, (size_t)t1->deco);
        else if (Expression *e1 = getExpression(o1))
            hash = mixHash(hash, expressionHash(e1));
        else if (Dsymbol *s1 = isDsymbol(o1))
        {
            FuncAliasDeclaration *fa1 = s1->isFuncAliasDeclaration();
            if (fa1)
                s1 = fa1->toAliasFunc();
            hash = mixHash(hash, mixHash((size_t)(void *)s1->getIdent(), (size_t)(void *)s1->parent));
        }
        else if (Tuple *u1 = isTuple(o1))
            hash = mixHash(hash, arrayObjectHash(&u1->objects));
    }
    return hash;
}

RootObject *objectSyntaxCopy(RootObject *o)
{
    if (!o)
        return NULL;
    if (Type *t = isType(o))
        return t->syntaxCopy();
    if (Expression *e = isExpression(o))
        return e->syntaxCopy();
    return o;
}


/* ======================== TemplateDeclaration ============================= */

TemplateDeclaration::TemplateDeclaration(Loc loc, Identifier *id,
        TemplateParameters *parameters, Expression *constraint, Dsymbols *decldefs, bool ismixin, bool literal)
    : ScopeDsymbol(id)
{
    this->loc = loc;
    this->parameters = parameters;
    this->origParameters = parameters;
    this->constraint = constraint;
    this->members = decldefs;
    this->overnext = NULL;
    this->overroot = NULL;
    this->funcroot = NULL;
    this->onemember = NULL;
    this->literal = literal;
    this->ismixin = ismixin;
    this->isstatic = true;
    this->previous = NULL;
    this->protection = Prot(PROTundefined);
    this->instances = NULL;

    // Compute in advance for Ddoc's use
    // Bugzilla 11153: ident could be NULL if parsing fails.
    if (members && ident)
    {
        Dsymbol *s;
        if (Dsymbol::oneMembers(members, &s, ident) && s)
        {
            onemember = s;
            s->parent = this;
        }
    }
}

Dsymbol *TemplateDeclaration::syntaxCopy(Dsymbol *)
{
    //printf("TemplateDeclaration::syntaxCopy()\n");
    TemplateParameters *p = NULL;
    if (parameters)
    {
        p = new TemplateParameters();
        p->setDim(parameters->dim);
        for (size_t i = 0; i < p->dim; i++)
            (*p)[i] = (*parameters)[i]->syntaxCopy();
    }
    return new TemplateDeclaration(loc, ident, p,
        constraint ? constraint->syntaxCopy() : NULL,
        Dsymbol::arraySyntaxCopy(members), ismixin, literal);
}

void TemplateDeclaration::semantic(Scope *sc)
{
    if (semanticRun != PASSinit)
        return;         // semantic() already run

    // Remember templates defined in module object that we need to know about
    if (sc->_module && sc->_module->ident == Id::object)
    {
        if (ident == Id::RTInfo)
            Type::rtinfo = this;
    }

    /* Remember Scope for later instantiations, but make
     * a copy since attributes can change.
     */
    if (!this->_scope)
    {
        this->_scope = sc->copy();
        this->_scope->setNoFree();
    }

    semanticRun = PASSsemantic;

    parent = sc->parent;
    protection = sc->protection;
    isstatic = toParent()->isModule() || (_scope->stc & STCstatic);

    if (!isstatic)
    {
        if (AggregateDeclaration *ad = parent->pastMixin()->isAggregateDeclaration())
            ad->makeNested();
    }

    // Set up scope for parameters
    ScopeDsymbol *paramsym = new ScopeDsymbol();
    paramsym->parent = parent;
    Scope *paramscope = sc->push(paramsym);
    paramscope->stc = 0;

    if (global.params.doDocComments)
    {
        origParameters = new TemplateParameters();
        origParameters->setDim(parameters->dim);
        for (size_t i = 0; i < parameters->dim; i++)
        {
            TemplateParameter *tp = (*parameters)[i];
            (*origParameters)[i] = tp->syntaxCopy();
        }
    }

    for (size_t i = 0; i < parameters->dim; i++)
    {
        TemplateParameter *tp = (*parameters)[i];

        if (!tp->declareParameter(paramscope))
        {
            error(tp->loc, "parameter '%s' multiply defined", tp->ident->toChars());
            errors = true;
        }
        if (!tp->semantic(paramscope, parameters))
        {
            errors = true;
        }
        if (i + 1 != parameters->dim && tp->isTemplateTupleParameter())
        {
            error("template tuple parameter must be last one");
            errors = true;
        }
    }

    /* Calculate TemplateParameter::dependent
     */
    TemplateParameters tparams;
    tparams.setDim(1);
    for (size_t i = 0; i < parameters->dim; i++)
    {
        TemplateParameter *tp = (*parameters)[i];
        tparams[0] = tp;

        for (size_t j = 0; j < parameters->dim; j++)
        {
            // Skip cases like: X(T : T)
            if (i == j)
                continue;

            if (TemplateTypeParameter *ttp = (*parameters)[j]->isTemplateTypeParameter())
            {
                if (reliesOnTident(ttp->specType, &tparams))
                    tp->dependent = true;
            }
            else if (TemplateAliasParameter *tap = (*parameters)[j]->isTemplateAliasParameter())
            {
                if (reliesOnTident(tap->specType, &tparams) ||
                    reliesOnTident(isType(tap->specAlias), &tparams))
                {
                    tp->dependent = true;
                }
            }
        }
    }

    paramscope->pop();

    // Compute again
    onemember = NULL;
    if (members)
    {
        Dsymbol *s;
        if (Dsymbol::oneMembers(members, &s, ident) && s)
        {
            onemember = s;
            s->parent = this;
        }
    }

    /* BUG: should check:
     *  o no virtual functions or non-static data members of classes
     */
    semanticRun = PASSsemanticdone;
}

const char *TemplateDeclaration::kind() const
{
    return (onemember && onemember->isAggregateDeclaration())
                ? onemember->kind()
                : "template";
}

/**********************************
 * Overload existing TemplateDeclaration 'this' with the new one 's'.
 * Return true if successful; i.e. no conflict.
 */

bool TemplateDeclaration::overloadInsert(Dsymbol *s)
{
    FuncDeclaration *fd = s->isFuncDeclaration();
    if (fd)
    {
        if (funcroot)
            return funcroot->overloadInsert(fd);
        funcroot = fd;
        return funcroot->overloadInsert(this);
    }

    TemplateDeclaration *td = s->isTemplateDeclaration();
    if (!td)
        return false;

    TemplateDeclaration *pthis = this;
    TemplateDeclaration **ptd;
    for (ptd = &pthis; *ptd; ptd = &(*ptd)->overnext)
    {
    }

    td->overroot = this;
    *ptd = td;
    return true;
}

/****************************
 * Check to see if constraint is satisfied.
 */
bool TemplateDeclaration::evaluateConstraint(
        TemplateInstance *ti, Scope *sc, Scope *paramscope,
        Objects *dedargs, FuncDeclaration *fd)
{
    /* Detect recursive attempts to instantiate this template declaration,
     * Bugzilla 4072
     *  void foo(T)(T x) if (is(typeof(foo(x)))) { }
     *  static assert(!is(typeof(foo(7))));
     * Recursive attempts are regarded as a constraint failure.
     */
    /* There's a chicken-and-egg problem here. We don't know yet if this template
     * instantiation will be a local one (enclosing is set), and we won't know until
     * after selecting the correct template. Thus, function we're nesting inside
     * is not on the sc scope chain, and this can cause errors in FuncDeclaration::getLevel().
     * Workaround the problem by setting a flag to relax the checking on frame errors.
     */

    for (TemplatePrevious *p = previous; p; p = p->prev)
    {
        if (arrayObjectMatch(p->dedargs, dedargs))
        {
            //printf("recursive, no match p->sc=%p %p %s\n", p->sc, this, this->toChars());
            /* It must be a subscope of p->sc, other scope chains are not recursive
             * instantiations.
             */
            for (Scope *scx = sc; scx; scx = scx->enclosing)
            {
                if (scx == p->sc)
                    return false;
            }
        }
        /* BUG: should also check for ref param differences
         */
    }

    TemplatePrevious pr;
    pr.prev    = previous;
    pr.sc      = paramscope;
    pr.dedargs = dedargs;
    previous = &pr;                 // add this to threaded list

    Scope *scx = paramscope->push(ti);
    scx->parent = ti;
    scx->tinst = NULL;
    scx->minst = NULL;

    assert(!ti->symtab);
    if (fd)
    {
        /* Declare all the function parameters as variables and add them to the scope
         * Making parameters is similar to FuncDeclaration::semantic3
         */
        TypeFunction *tf = (TypeFunction *)fd->type;
        assert(tf->ty == Tfunction);

        scx->parent = fd;

        Parameters *fparameters = tf->parameters;
        int fvarargs = tf->varargs;

        size_t nfparams = Parameter::dim(fparameters);
        for (size_t i = 0; i < nfparams; i++)
        {
            Parameter *fparam = Parameter::getNth(fparameters, i);
            fparam->storageClass &= (STCin | STCout | STCref | STClazy | STCfinal | STC_TYPECTOR | STCnodtor);
            fparam->storageClass |= STCparameter;
            if (fvarargs == 2 && i + 1 == nfparams)
                fparam->storageClass |= STCvariadic;
        }
        for (size_t i = 0; i < fparameters->dim; i++)
        {
            Parameter *fparam = (*fparameters)[i];
            if (!fparam->ident)
                continue;                       // don't add it, if it has no name
            VarDeclaration *v = new VarDeclaration(loc, fparam->type, fparam->ident, NULL);
            v->storage_class = fparam->storageClass;
            v->semantic(scx);
            if (!ti->symtab)
                ti->symtab = new DsymbolTable();
            if (!scx->insert(v))
                error("parameter %s.%s is already defined", toChars(), v->toChars());
            else
                v->parent = fd;
        }
        if (isstatic)
            fd->storage_class |= STCstatic;

        fd->vthis = fd->declareThis(scx, fd->isThis());
    }

    Expression *e = constraint->syntaxCopy();

    assert(ti->inst == NULL);
    ti->inst = ti;  // temporary instantiation to enable genIdent()

    scx->flags |= SCOPEconstraint;
    bool errors = false;
    bool result = evalStaticCondition(scx, constraint, e, errors);
    ti->inst = NULL;
    ti->symtab = NULL;
    scx = scx->pop();
    previous = pr.prev;             // unlink from threaded list
    if (errors)
        return false;
    return result;
}

/***************************************
 * Given that ti is an instance of this TemplateDeclaration,
 * deduce the types of the parameters to this, and store
 * those deduced types in dedtypes[].
 * Input:
 *      flag    1: don't do semantic() because of dummy types
 *              2: don't change types in matchArg()
 * Output:
 *      dedtypes        deduced arguments
 * Return match level.
 */

MATCH TemplateDeclaration::matchWithInstance(Scope *sc, TemplateInstance *ti,
        Objects *dedtypes, Expressions *fargs, int flag)
{
    MATCH m;
    size_t dedtypes_dim = dedtypes->dim;

    dedtypes->zero();

    if (errors)
        return MATCHnomatch;

    size_t parameters_dim = parameters->dim;
    int variadic = isVariadic() != NULL;

    // If more arguments than parameters, no match
    if (ti->tiargs->dim > parameters_dim && !variadic)
    {
        return MATCHnomatch;
    }

    assert(dedtypes_dim == parameters_dim);
    assert(dedtypes_dim >= ti->tiargs->dim || variadic);

    assert(_scope);

    // Set up scope for template parameters
    ScopeDsymbol *paramsym = new ScopeDsymbol();
    paramsym->parent = _scope->parent;
    Scope *paramscope = _scope->push(paramsym);
    paramscope->tinst = ti;
    paramscope->minst = sc->minst;
    paramscope->callsc = sc;
    paramscope->stc = 0;

    // Attempt type deduction
    m = MATCHexact;
    for (size_t i = 0; i < dedtypes_dim; i++)
    {
        MATCH m2;
        TemplateParameter *tp = (*parameters)[i];
        Declaration *sparam;

        //printf("\targument [%d]\n", i);
        m2 = tp->matchArg(ti->loc, paramscope, ti->tiargs, i, parameters, dedtypes, &sparam);
        //printf("\tm2 = %d\n", m2);

        if (m2 == MATCHnomatch)
        {
            goto Lnomatch;
        }

        if (m2 < m)
            m = m2;

        if (!flag)
            sparam->semantic(paramscope);
        if (!paramscope->insert(sparam))    // TODO: This check can make more early
            goto Lnomatch;                  // in TemplateDeclaration::semantic, and
                                            // then we don't need to make sparam if flags == 0
    }

    if (!flag)
    {
        /* Any parameter left without a type gets the type of
         * its corresponding arg
         */
        for (size_t i = 0; i < dedtypes_dim; i++)
        {
            if (!(*dedtypes)[i])
            {
                assert(i < ti->tiargs->dim);
                (*dedtypes)[i] = (Type *)(*ti->tiargs)[i];
            }
        }
    }

    if (m > MATCHnomatch && constraint && !flag)
    {
        if (ti->hasNestedArgs(ti->tiargs, this->isstatic))  // TODO: should gag error
            ti->parent = ti->enclosing;
        else
            ti->parent = this->parent;

        // Similar to doHeaderInstantiation
        FuncDeclaration *fd = onemember ? onemember->isFuncDeclaration() : NULL;
        if (fd)
        {
            assert(fd->type->ty == Tfunction);
            TypeFunction *tf = (TypeFunction *)fd->type->syntaxCopy();

            fd = new FuncDeclaration(fd->loc, fd->endloc, fd->ident, fd->storage_class, tf);
            fd->parent = ti;
            fd->inferRetType = true;

            // Shouldn't run semantic on default arguments and return type.
            for (size_t i = 0; i < tf->parameters->dim; i++)
                (*tf->parameters)[i]->defaultArg = NULL;
            tf->next = NULL;

            // Resolve parameter types and 'auto ref's.
            tf->fargs = fargs;
            unsigned olderrors = global.startGagging();
            fd->type = tf->semantic(loc, paramscope);
            if (global.endGagging(olderrors))
            {
                assert(fd->type->ty != Tfunction);
                goto Lnomatch;
            }
            assert(fd->type->ty == Tfunction);
            fd->originalType = fd->type;    // for mangling
        }

        // TODO: dedtypes => ti->tiargs ?
        if (!evaluateConstraint(ti, sc, paramscope, dedtypes, fd))
            goto Lnomatch;
    }

    goto Lret;

Lnomatch:
    m = MATCHnomatch;

Lret:
    paramscope->pop();
    return m;
}

/********************************************
 * Determine partial specialization order of 'this' vs td2.
 * Returns:
 *      match   this is at least as specialized as td2
 *      0       td2 is more specialized than this
 */

MATCH TemplateDeclaration::leastAsSpecialized(Scope *sc, TemplateDeclaration *td2, Expressions *fargs)
{
    /* This works by taking the template parameters to this template
     * declaration and feeding them to td2 as if it were a template
     * instance.
     * If it works, then this template is at least as specialized
     * as td2.
     */

    TemplateInstance ti(Loc(), ident);      // create dummy template instance
    // Set type arguments to dummy template instance to be types
    // generated from the parameters to this template declaration
    ti.tiargs = new Objects();
    ti.tiargs->reserve(parameters->dim);
    for (size_t i = 0; i < parameters->dim; i++)
    {
        TemplateParameter *tp = (*parameters)[i];
        if (tp->dependent)
            break;
        RootObject *p = (RootObject *)tp->dummyArg();
        if (!p)
            break;

        ti.tiargs->push(p);
    }

    // Temporary Array to hold deduced types
    Objects dedtypes;
    dedtypes.setDim(td2->parameters->dim);

    // Attempt a type deduction
    MATCH m = td2->matchWithInstance(sc, &ti, &dedtypes, fargs, 1);
    if (m > MATCHnomatch)
    {
        /* A non-variadic template is more specialized than a
         * variadic one.
         */
        TemplateTupleParameter *tp = isVariadic();
        if (tp && !tp->dependent && !td2->isVariadic())
            goto L1;

        return m;
    }
  L1:
    return MATCHnomatch;
}

static Expression *emptyArrayElement = NULL;

class TypeDeduced : public Type
{
public:
    Type *tded;
    Expressions argexps;    // corresponding expressions
    Types tparams;          // tparams[i]->mod

    TypeDeduced(Type *tt, Expression *e, Type *tparam)
        : Type(Tnone)
    {
        tded = tt;
        argexps.push(e);
        tparams.push(tparam);
    }

    virtual ~TypeDeduced()
    {
    }

    void update(Expression *e, Type *tparam)
    {
        argexps.push(e);
        tparams.push(tparam);
    }
    void update(Type *tt, Expression *e, Type *tparam)
    {
        tded = tt;
        argexps.push(e);
        tparams.push(tparam);
    }
    MATCH matchAll(Type *tt)
    {
        MATCH match = MATCHexact;
        for (size_t j = 0; j < argexps.dim; j++)
        {
            Expression *e = argexps[j];
            assert(e);
            if (e == emptyArrayElement)
                continue;

            Type *t = tt->addMod(tparams[j]->mod)->substWildTo(MODconst);

            MATCH m = e->implicitConvTo(t);
            if (match > m)
                match = m;
            if (match <= MATCHnomatch)
                break;
        }
        return match;
    }
};

/*************************************************
 * Match function arguments against a specific template function.
 * Input:
 *      ti
 *      sc              instantiation scope
 *      fd
 *      tthis           'this' argument if !NULL
 *      fargs           arguments to function
 * Output:
 *      fd              Partially instantiated function declaration
 *      ti->tdtypes     Expression/Type deduced template arguments
 * Returns:
 *      match level
 *          bit 0-3     Match template parameters by inferred template arguments
 *          bit 4-7     Match template parameters by initial template arguments
 */

MATCH TemplateDeclaration::deduceFunctionTemplateMatch(
        TemplateInstance *ti, Scope *sc,
        FuncDeclaration *&fd, Type *tthis, Expressions *fargs)
{
    size_t nfparams;
    size_t nfargs;
    size_t ntargs;              // array size of tiargs
    size_t fptupindex = IDX_NOTFOUND;
    MATCH match = MATCHexact;
    MATCH matchTiargs = MATCHexact;
    Parameters *fparameters;            // function parameter list
    int fvarargs;                       // function varargs
    unsigned wildmatch = 0;
    size_t inferStart = 0;

    Loc instLoc = ti->loc;
    Objects *tiargs = ti->tiargs;
    Objects *dedargs = new Objects();
    Objects* dedtypes = &ti->tdtypes;   // for T:T*, the dedargs is the T*, dedtypes is the T

    assert(_scope);

    dedargs->setDim(parameters->dim);
    dedargs->zero();

    dedtypes->setDim(parameters->dim);
    dedtypes->zero();

    if (errors || fd->errors)
        return MATCHnomatch;

    // Set up scope for parameters
    ScopeDsymbol *paramsym = new ScopeDsymbol();
    paramsym->parent = _scope->parent;   // should use hasnestedArgs and enclosing?
    Scope *paramscope = _scope->push(paramsym);
    paramscope->tinst = ti;
    paramscope->minst = sc->minst;
    paramscope->callsc = sc;
    paramscope->stc = 0;

    TemplateTupleParameter *tp = isVariadic();
    Tuple *declaredTuple = NULL;

    ntargs = 0;
    if (tiargs)
    {
        // Set initial template arguments
        ntargs = tiargs->dim;
        size_t n = parameters->dim;
        if (tp)
            n--;
        if (ntargs > n)
        {
            if (!tp)
                goto Lnomatch;

            /* The extra initial template arguments
             * now form the tuple argument.
             */
            Tuple *t = new Tuple();
            assert(parameters->dim);
            (*dedargs)[parameters->dim - 1] = t;

            t->objects.setDim(ntargs - n);
            for (size_t i = 0; i < t->objects.dim; i++)
            {
                t->objects[i] = (*tiargs)[n + i];
            }
            declareParameter(paramscope, tp, t);
            declaredTuple = t;
        }
        else
            n = ntargs;

        memcpy(dedargs->tdata(), tiargs->tdata(), n * sizeof(*dedargs->tdata()));

        for (size_t i = 0; i < n; i++)
        {
            assert(i < parameters->dim);
            Declaration *sparam = NULL;
            MATCH m = (*parameters)[i]->matchArg(instLoc, paramscope, dedargs, i, parameters, dedtypes, &sparam);
            //printf("\tdeduceType m = %d\n", m);
            if (m <= MATCHnomatch)
                goto Lnomatch;
            if (m < matchTiargs)
                matchTiargs = m;

            sparam->semantic(paramscope);
            if (!paramscope->insert(sparam))
                goto Lnomatch;
        }
        if (n < parameters->dim && !declaredTuple)
        {
            inferStart = n;
        }
        else
            inferStart = parameters->dim;
        //printf("tiargs matchTiargs = %d\n", matchTiargs);
    }

    fparameters = fd->getParameters(&fvarargs);
    nfparams = Parameter::dim(fparameters);     // number of function parameters
    nfargs = fargs ? fargs->dim : 0;            // number of function arguments

    /* Check for match of function arguments with variadic template
     * parameter, such as:
     *
     * void foo(T, A...)(T t, A a);
     * void main() { foo(1,2,3); }
     */
    if (tp)                             // if variadic
    {
        // TemplateTupleParameter always makes most lesser matching.
        matchTiargs = MATCHconvert;

        if (nfparams == 0 && nfargs != 0)               // if no function parameters
        {
            if (!declaredTuple)
            {
                Tuple *t = new Tuple();
                //printf("t = %p\n", t);
                (*dedargs)[parameters->dim - 1] = t;
                declareParameter(paramscope, tp, t);
                declaredTuple = t;
            }
        }
        else
        {
            /* Figure out which of the function parameters matches
             * the tuple template parameter. Do this by matching
             * type identifiers.
             * Set the index of this function parameter to fptupindex.
             */
            for (fptupindex = 0; fptupindex < nfparams; fptupindex++)
            {
                Parameter *fparam = (*fparameters)[fptupindex];
                if (fparam->type->ty != Tident)
                    continue;
                TypeIdentifier *tid = (TypeIdentifier *)fparam->type;
                if (!tp->ident->equals(tid->ident) || tid->idents.dim)
                    continue;

                if (fvarargs)           // variadic function doesn't
                    goto Lnomatch;      // go with variadic template

                goto L1;
            }
            fptupindex = IDX_NOTFOUND;
        L1:
            ;
        }
    }

    if (toParent()->isModule() || (_scope->stc & STCstatic))
        tthis = NULL;
    if (tthis)
    {
        bool hasttp = false;

        // Match 'tthis' to any TemplateThisParameter's
        for (size_t i = 0; i < parameters->dim; i++)
        {
            TemplateThisParameter *ttp = (*parameters)[i]->isTemplateThisParameter();
            if (ttp)
            {
                hasttp = true;

                Type *t = new TypeIdentifier(Loc(), ttp->ident);
                MATCH m = deduceType(tthis, paramscope, t, parameters, dedtypes);
                if (m <= MATCHnomatch)
                    goto Lnomatch;
                if (m < match)
                    match = m;          // pick worst match
            }
        }

        // Match attributes of tthis against attributes of fd
        if (fd->type && !fd->isCtorDeclaration())
        {
            StorageClass stc = _scope->stc | fd->storage_class2;
            // Propagate parent storage class (see bug 5504)
            Dsymbol *p = parent;
            while (p->isTemplateDeclaration() || p->isTemplateInstance())
                p = p->parent;
            AggregateDeclaration *ad = p->isAggregateDeclaration();
            if (ad)
                stc |= ad->storage_class;

            unsigned char mod = fd->type->mod;
            if (stc & STCimmutable)
                mod = MODimmutable;
            else
            {
                if (stc & (STCshared | STCsynchronized))
                    mod |= MODshared;
                if (stc & STCconst)
                    mod |= MODconst;
                if (stc & STCwild)
                    mod |= MODwild;
            }

            unsigned char thismod = tthis->mod;
            if (hasttp)
                mod = MODmerge(thismod, mod);
            MATCH m = MODmethodConv(thismod, mod);
            if (m <= MATCHnomatch)
                goto Lnomatch;
            if (m < match)
                match = m;
        }
    }

    // Loop through the function parameters
    {
    //printf("%s\n\tnfargs = %d, nfparams = %d, tuple_dim = %d\n", toChars(), nfargs, nfparams, declaredTuple ? declaredTuple->objects.dim : 0);
    //printf("\ttp = %p, fptupindex = %d, found = %d, declaredTuple = %s\n", tp, fptupindex, fptupindex != IDX_NOTFOUND, declaredTuple ? declaredTuple->toChars() : NULL);
    size_t argi = 0;
    size_t nfargs2 = nfargs;    // nfargs + supplied defaultArgs
    for (size_t parami = 0; parami < nfparams; parami++)
    {
        Parameter *fparam = Parameter::getNth(fparameters, parami);

        // Apply function parameter storage classes to parameter types
        Type *prmtype = fparam->type->addStorageClass(fparam->storageClass);

        Expression *farg;

        /* See function parameters which wound up
         * as part of a template tuple parameter.
         */
        if (fptupindex != IDX_NOTFOUND && parami == fptupindex)
        {
            assert(prmtype->ty == Tident);
            TypeIdentifier *tid = (TypeIdentifier *)prmtype;
            if (!declaredTuple)
            {
                /* The types of the function arguments
                 * now form the tuple argument.
                 */
                declaredTuple = new Tuple();
                (*dedargs)[parameters->dim - 1] = declaredTuple;

                /* Count function parameters following a tuple parameter.
                 * void foo(U, T...)(int y, T, U, int) {}  // rem == 2 (U, int)
                 */
                size_t rem = 0;
                for (size_t j = parami + 1; j < nfparams; j++)
                {
                    Parameter *p = Parameter::getNth(fparameters, j);
                    if (!reliesOnTident(p->type, parameters, inferStart))
                    {
                        Type *pt = p->type->syntaxCopy()->semantic(fd->loc, paramscope);
                        rem += pt->ty == Ttuple ? ((TypeTuple *)pt)->arguments->dim : 1;
                    }
                    else
                    {
                        ++rem;
                    }
                }

                if (nfargs2 - argi < rem)
                    goto Lnomatch;
                declaredTuple->objects.setDim(nfargs2 - argi - rem);
                for (size_t i = 0; i < declaredTuple->objects.dim; i++)
                {
                    farg = (*fargs)[argi + i];

                    // Check invalid arguments to detect errors early.
                    if (farg->op == TOKerror || farg->type->ty == Terror)
                        goto Lnomatch;

                    if (!(fparam->storageClass & STClazy) && farg->type->ty == Tvoid)
                        goto Lnomatch;

                    Type *tt;
                    MATCH m;
                    if (unsigned char wm = deduceWildHelper(farg->type, &tt, tid))
                    {
                        wildmatch |= wm;
                        m = MATCHconst;
                    }
                    else
                    {
                        m = deduceTypeHelper(farg->type, &tt, tid);
                    }
                    if (m <= MATCHnomatch)
                        goto Lnomatch;
                    if (m < match)
                        match = m;

                    /* Remove top const for dynamic array types and pointer types
                     */
                    if ((tt->ty == Tarray || tt->ty == Tpointer) &&
                        !tt->isMutable() &&
                        (!(fparam->storageClass & STCref) ||
                         ((fparam->storageClass & STCauto) && !farg->isLvalue())))
                    {
                        tt = tt->mutableOf();
                    }
                    declaredTuple->objects[i] = tt;
                }
                declareParameter(paramscope, tp, declaredTuple);
            }
            else
            {
                // Bugzilla 6810: If declared tuple is not a type tuple,
                // it cannot be function parameter types.
                for (size_t i = 0; i < declaredTuple->objects.dim; i++)
                {
                    if (!isType(declaredTuple->objects[i]))
                        goto Lnomatch;
                }
            }
            assert(declaredTuple);
            argi += declaredTuple->objects.dim;
            continue;
        }

        // If parameter type doesn't depend on inferred template parameters,
        // semantic it to get actual type.
        if (!reliesOnTident(prmtype, parameters, inferStart))
        {
            // should copy prmtype to avoid affecting semantic result
            prmtype = prmtype->syntaxCopy()->semantic(fd->loc, paramscope);

            if (prmtype->ty == Ttuple)
            {
                TypeTuple *tt = (TypeTuple *)prmtype;
                size_t tt_dim = tt->arguments->dim;
                for (size_t j = 0; j < tt_dim; j++, ++argi)
                {
                    Parameter *p = (*tt->arguments)[j];
                    if (j == tt_dim - 1 && fvarargs == 2 && parami + 1 == nfparams && argi < nfargs)
                    {
                        prmtype = p->type;
                        goto Lvarargs;
                    }
                    if (argi >= nfargs)
                    {
                        if (p->defaultArg)
                            continue;
                        goto Lnomatch;
                    }
                    farg = (*fargs)[argi];
                    if (!farg->implicitConvTo(p->type))
                        goto Lnomatch;
                }
                continue;
            }
        }

        if (argi >= nfargs)                // if not enough arguments
        {
            if (!fparam->defaultArg)
                goto Lvarargs;

            /* Bugzilla 2803: Before the starting of type deduction from the function
             * default arguments, set the already deduced parameters into paramscope.
             * It's necessary to avoid breaking existing acceptable code. Cases:
             *
             * 1. Already deduced template parameters can appear in fparam->defaultArg:
             *  auto foo(A, B)(A a, B b = A.stringof);
             *  foo(1);
             *  // at fparam == 'B b = A.string', A is equivalent with the deduced type 'int'
             *
             * 2. If prmtype depends on default-specified template parameter, the
             * default type should be preferred.
             *  auto foo(N = size_t, R)(R r, N start = 0)
             *  foo([1,2,3]);
             *  // at fparam `N start = 0`, N should be 'size_t' before
             *  // the deduction result from fparam->defaultArg.
             */
            if (argi == nfargs)
            {
                for (size_t i = 0; i < dedtypes->dim; i++)
                {
                    Type *at = isType((*dedtypes)[i]);
                    if (at && at->ty == Tnone)
                    {
                        TypeDeduced *xt = (TypeDeduced *)at;
                        (*dedtypes)[i] = xt->tded;  // 'unbox'
                        delete xt;
                    }
                }
                for (size_t i = ntargs; i < dedargs->dim; i++)
                {
                    TemplateParameter *tparam = (*parameters)[i];

                    RootObject *oarg = (*dedargs)[i];
                    RootObject *oded = (*dedtypes)[i];
                    if (!oarg)
                    {
                        if (oded)
                        {
                            if (tparam->specialization() || !tparam->isTemplateTypeParameter())
                            {
                                /* The specialization can work as long as afterwards
                                 * the oded == oarg
                                 */
                                (*dedargs)[i] = oded;
                                MATCH m2 = tparam->matchArg(instLoc, paramscope, dedargs, i, parameters, dedtypes, NULL);
                                //printf("m2 = %d\n", m2);
                                if (m2 <= MATCHnomatch)
                                    goto Lnomatch;
                                if (m2 < matchTiargs)
                                    matchTiargs = m2;             // pick worst match
                                if (!(*dedtypes)[i]->equals(oded))
                                    error("specialization not allowed for deduced parameter %s", tparam->ident->toChars());
                            }
                            else
                            {
                                if (MATCHconvert < matchTiargs)
                                    matchTiargs = MATCHconvert;
                            }
                            (*dedargs)[i] = declareParameter(paramscope, tparam, oded);
                        }
                        else
                        {
                            oded = tparam->defaultArg(instLoc, paramscope);
                            if (oded)
                                (*dedargs)[i] = declareParameter(paramscope, tparam, oded);
                        }
                    }
                }
            }
            nfargs2 = argi + 1;

            /* If prmtype does not depend on any template parameters:
             *
             *  auto foo(T)(T v, double x = 0);
             *  foo("str");
             *  // at fparam == 'double x = 0'
             *
             * or, if all template parameters in the prmtype are already deduced:
             *
             *  auto foo(R)(R range, ElementType!R sum = 0);
             *  foo([1,2,3]);
             *  // at fparam == 'ElementType!R sum = 0'
             *
             * Deducing prmtype from fparam->defaultArg is not necessary.
             */
            if (prmtype->deco ||
                prmtype->syntaxCopy()->trySemantic(loc, paramscope))
            {
                ++argi;
                continue;
            }

            // Deduce prmtype from the defaultArg.
            farg = fparam->defaultArg->syntaxCopy();
            farg = ::semantic(farg, paramscope);
            farg = resolveProperties(paramscope, farg);
        }
        else
        {
            farg = (*fargs)[argi];
        }
        {
            // Check invalid arguments to detect errors early.
            if (farg->op == TOKerror || farg->type->ty == Terror)
                goto Lnomatch;

            Type *att = NULL;
        Lretry:
            Type *argtype = farg->type;

            if (!(fparam->storageClass & STClazy) && argtype->ty == Tvoid && farg->op != TOKfunction)
                goto Lnomatch;

            // Bugzilla 12876: optimize arugument to allow CT-known length matching
            farg = farg->optimize(WANTvalue, (fparam->storageClass & (STCref | STCout)) != 0);
            //printf("farg = %s %s\n", farg->type->toChars(), farg->toChars());

            RootObject *oarg = farg;
            if ((fparam->storageClass & STCref) &&
                (!(fparam->storageClass & STCauto) || farg->isLvalue()))
            {
                /* Allow expressions that have CT-known boundaries and type [] to match with [dim]
                 */
                Type *taai;
                if (argtype->ty == Tarray &&
                    (prmtype->ty == Tsarray ||
                     (prmtype->ty == Taarray && (taai = ((TypeAArray *)prmtype)->index)->ty == Tident &&
                      ((TypeIdentifier *)taai)->idents.dim == 0)))
                {
                    if (farg->op == TOKstring)
                    {
                        StringExp *se = (StringExp *)farg;
                        argtype = se->type->nextOf()->sarrayOf(se->len);
                    }
                    else if (farg->op == TOKarrayliteral)
                    {
                        ArrayLiteralExp *ae = (ArrayLiteralExp *)farg;
                        argtype = ae->type->nextOf()->sarrayOf(ae->elements->dim);
                    }
                    else if (farg->op == TOKslice)
                    {
                        SliceExp *se = (SliceExp *)farg;
                        if (Type *tsa = toStaticArrayType(se))
                            argtype = tsa;
                    }
                }

                oarg = argtype;
            }
            else if ((fparam->storageClass & STCout) == 0 &&
                     (argtype->ty == Tarray || argtype->ty == Tpointer) &&
                     templateParameterLookup(prmtype, parameters) != IDX_NOTFOUND &&
                     ((TypeIdentifier *)prmtype)->idents.dim == 0)
            {
                /* The farg passing to the prmtype always make a copy. Therefore,
                 * we can shrink the set of the deduced type arguments for prmtype
                 * by adjusting top-qualifier of the argtype.
                 *
                 *  prmtype         argtype     ta
                 *  T            <- const(E)[]  const(E)[]
                 *  T            <- const(E[])  const(E)[]
                 *  qualifier(T) <- const(E)[]  const(E[])
                 *  qualifier(T) <- const(E[])  const(E[])
                 */
                Type *ta = argtype->castMod(prmtype->mod ? argtype->nextOf()->mod : 0);
                if (ta != argtype)
                {
                    Expression *ea = farg->copy();
                    ea->type = ta;
                    oarg = ea;
                }
            }

            if (fvarargs == 2 && parami + 1 == nfparams && argi + 1 < nfargs)
                goto Lvarargs;

            unsigned wm = 0;
            MATCH m = deduceType(oarg, paramscope, prmtype, parameters, dedtypes, &wm, inferStart);
            //printf("\tL%d deduceType m = %d, wm = x%x, wildmatch = x%x\n", __LINE__, m, wm, wildmatch);
            wildmatch |= wm;

            /* If no match, see if the argument can be matched by using
             * implicit conversions.
             */
            if (m == MATCHnomatch && prmtype->deco)
                m = farg->implicitConvTo(prmtype);

            if (m == MATCHnomatch)
            {
                AggregateDeclaration *ad = isAggregate(farg->type);
                if (ad && ad->aliasthis && argtype != att)
                {
                    if (!att && argtype->checkAliasThisRec())   // Bugzilla 12537
                        att = argtype;

                    /* If a semantic error occurs while doing alias this,
                     * eg purity(bug 7295), just regard it as not a match.
                     */
                    if (Expression *e = resolveAliasThis(sc, farg, true))
                    {
                        farg = e;
                        goto Lretry;
                    }
                }
            }

            if (m > MATCHnomatch && (fparam->storageClass & (STCref | STCauto)) == STCref)
            {
                if (!farg->isLvalue())
                {
                    if ((farg->op == TOKstring || farg->op == TOKslice) &&
                        (prmtype->ty == Tsarray || prmtype->ty == Taarray))
                    {
                        // Allow conversion from T[lwr .. upr] to ref T[upr-lwr]
                    }
                    else
                        goto Lnomatch;
                }
            }
            if (m > MATCHnomatch && (fparam->storageClass & STCout))
            {
                if (!farg->isLvalue())
                    goto Lnomatch;
                if (!farg->type->isMutable())   // Bugzilla 11916
                    goto Lnomatch;
            }
            if (m == MATCHnomatch && (fparam->storageClass & STClazy) && prmtype->ty == Tvoid &&
                    farg->type->ty != Tvoid)
                m = MATCHconvert;

            if (m != MATCHnomatch)
            {
                if (m < match)
                    match = m;          // pick worst match
                argi++;
                continue;
            }
        }

    Lvarargs:
        /* The following code for variadic arguments closely
         * matches TypeFunction::callMatch()
         */
        if (!(fvarargs == 2 && parami + 1 == nfparams))
            goto Lnomatch;

        /* Check for match with function parameter T...
         */
        Type *tb = prmtype->toBasetype();
        switch (tb->ty)
        {
            // 6764 fix - TypeAArray may be TypeSArray have not yet run semantic().
            case Tsarray:
            case Taarray:
                // Perhaps we can do better with this, see TypeFunction::callMatch()
                if (tb->ty == Tsarray)
                {
                    TypeSArray *tsa = (TypeSArray *)tb;
                    dinteger_t sz = tsa->dim->toInteger();
                    if (sz != nfargs - argi)
                        goto Lnomatch;
                }
                else if (tb->ty == Taarray)
                {
                    TypeAArray *taa = (TypeAArray *)tb;
                    Expression *dim = new IntegerExp(instLoc, nfargs - argi, Type::tsize_t);

                    size_t i = templateParameterLookup(taa->index, parameters);
                    if (i == IDX_NOTFOUND)
                    {
                        Expression *e;
                        Type *t;
                        Dsymbol *s;
                        Scope *sco;

                        unsigned errors = global.startGagging();
                        /* ref: https://issues.dlang.org/show_bug.cgi?id=11118
                         * The parameter isn't part of the template
                         * ones, let's try to find it in the
                         * instantiation scope 'sc' and the one
                         * belonging to the template itself. */
                        sco = sc;
                        taa->index->resolve(instLoc, sco, &e, &t, &s);
                        if (!e)
                        {
                            sco = paramscope;
                            taa->index->resolve(instLoc, sco, &e, &t, &s);
                        }
                        global.endGagging(errors);

                        if (!e)
                        {
                            goto Lnomatch;
                        }

                        e = e->ctfeInterpret();
                        e = e->implicitCastTo(sco, Type::tsize_t);
                        e = e->optimize(WANTvalue);
                        if (!dim->equals(e))
                            goto Lnomatch;
                    }
                    else
                    {
                        // This code matches code in TypeInstance::deduceType()
                        TemplateParameter *tprm = (*parameters)[i];
                        TemplateValueParameter *tvp = tprm->isTemplateValueParameter();
                        if (!tvp)
                            goto Lnomatch;
                        Expression *e = (Expression *)(*dedtypes)[i];
                        if (e)
                        {
                            if (!dim->equals(e))
                                goto Lnomatch;
                        }
                        else
                        {
                            Type *vt = tvp->valType->semantic(Loc(), sc);
                            MATCH m = (MATCH)dim->implicitConvTo(vt);
                            if (m <= MATCHnomatch)
                                goto Lnomatch;
                            (*dedtypes)[i] = dim;
                        }
                    }
                }
                /* fall through */
            case Tarray:
            {
                TypeArray *ta = (TypeArray *)tb;
                Type *tret = fparam->isLazyArray();
                for (; argi < nfargs; argi++)
                {
                    Expression *arg = (*fargs)[argi];
                    assert(arg);

                    MATCH m;
                    /* If lazy array of delegates,
                     * convert arg(s) to delegate(s)
                     */
                    if (tret)
                    {
                        if (ta->next->equals(arg->type))
                        {
                            m = MATCHexact;
                        }
                        else
                        {
                            m = arg->implicitConvTo(tret);
                            if (m == MATCHnomatch)
                            {
                                if (tret->toBasetype()->ty == Tvoid)
                                    m = MATCHconvert;
                            }
                        }
                    }
                    else
                    {
                        unsigned wm = 0;
                        m = deduceType(arg, paramscope, ta->next, parameters, dedtypes, &wm, inferStart);
                        wildmatch |= wm;
                    }
                    if (m == MATCHnomatch)
                        goto Lnomatch;
                    if (m < match)
                        match = m;
                }
                goto Lmatch;
            }
            case Tclass:
            case Tident:
                goto Lmatch;

            default:
                goto Lnomatch;
        }
        ++argi;
    }
    //printf("-> argi = %d, nfargs = %d, nfargs2 = %d\n", argi, nfargs, nfargs2);
    if (argi != nfargs2 && !fvarargs)
        goto Lnomatch;
    }

Lmatch:

    for (size_t i = 0; i < dedtypes->dim; i++)
    {
        Type *at = isType((*dedtypes)[i]);
        if (at)
        {
            if (at->ty == Tnone)
            {
                TypeDeduced *xt = (TypeDeduced *)at;
                at = xt->tded;  // 'unbox'
                delete xt;
            }
            (*dedtypes)[i] = at->merge2();
        }
    }
    for (size_t i = ntargs; i < dedargs->dim; i++)
    {
        TemplateParameter *tparam = (*parameters)[i];
        //printf("tparam[%d] = %s\n", i, tparam->ident->toChars());
        /* For T:T*, the dedargs is the T*, dedtypes is the T
         * But for function templates, we really need them to match
         */
        RootObject *oarg = (*dedargs)[i];
        RootObject *oded = (*dedtypes)[i];
        //printf("1dedargs[%d] = %p, dedtypes[%d] = %p\n", i, oarg, i, oded);
        //if (oarg) printf("oarg: %s\n", oarg->toChars());
        //if (oded) printf("oded: %s\n", oded->toChars());
        if (!oarg)
        {
            if (oded)
            {
                if (tparam->specialization() || !tparam->isTemplateTypeParameter())
                {
                    /* The specialization can work as long as afterwards
                     * the oded == oarg
                     */
                    (*dedargs)[i] = oded;
                    MATCH m2 = tparam->matchArg(instLoc, paramscope, dedargs, i, parameters, dedtypes, NULL);
                    //printf("m2 = %d\n", m2);
                    if (m2 <= MATCHnomatch)
                        goto Lnomatch;
                    if (m2 < matchTiargs)
                        matchTiargs = m2;             // pick worst match
                    if (!(*dedtypes)[i]->equals(oded))
                        error("specialization not allowed for deduced parameter %s", tparam->ident->toChars());
                }
                else
                {
                    if (MATCHconvert < matchTiargs)
                        matchTiargs = MATCHconvert;
                }
            }
            else
            {
                oded = tparam->defaultArg(instLoc, paramscope);
                if (!oded)
                {
                    // if tuple parameter and
                    // tuple parameter was not in function parameter list and
                    // we're one or more arguments short (i.e. no tuple argument)
                    if (tparam == tp &&
                        fptupindex == IDX_NOTFOUND &&
                        ntargs <= dedargs->dim - 1)
                    {
                        // make tuple argument an empty tuple
                        oded = (RootObject *)new Tuple();
                    }
                    else
                        goto Lnomatch;
                }
                if (isError(oded))
                    goto Lerror;
                ntargs++;

                /* At the template parameter T, the picked default template argument
                 * X!int should be matched to T in order to deduce dependent
                 * template parameter A.
                 *  auto foo(T : X!A = X!int, A...)() { ... }
                 *  foo();  // T <-- X!int, A <-- (int)
                 */
                if (tparam->specialization())
                {
                    (*dedargs)[i] = oded;
                    MATCH m2 = tparam->matchArg(instLoc, paramscope, dedargs, i, parameters, dedtypes, NULL);
                    //printf("m2 = %d\n", m2);
                    if (m2 <= MATCHnomatch)
                        goto Lnomatch;
                    if (m2 < matchTiargs)
                        matchTiargs = m2;             // pick worst match
                    if (!(*dedtypes)[i]->equals(oded))
                        error("specialization not allowed for deduced parameter %s", tparam->ident->toChars());
                }
            }
            oded = declareParameter(paramscope, tparam, oded);
            (*dedargs)[i] = oded;
        }
    }

    /* Bugzilla 7469: As same as the code for 7469 in findBestMatch,
     * expand a Tuple in dedargs to normalize template arguments.
     */
    if (size_t d = dedargs->dim)
    {
        if (Tuple *va = isTuple((*dedargs)[d - 1]))
        {
            if (va->objects.dim)
            {
                dedargs->setDim(d - 1);
                dedargs->insert(d - 1, &va->objects);
            }
        }
    }
    ti->tiargs = dedargs; // update to the normalized template arguments.

    // Partially instantiate function for constraint and fd->leastAsSpecialized()
    {
        assert(paramsym);
        Scope *sc2 = _scope;
        sc2 = sc2->push(paramsym);
        sc2 = sc2->push(ti);
        sc2->parent = ti;
        sc2->tinst = ti;
        sc2->minst = sc->minst;

        fd = doHeaderInstantiation(ti, sc2, fd, tthis, fargs);

        sc2 = sc2->pop();
        sc2 = sc2->pop();

        if (!fd)
            goto Lnomatch;
    }

    if (constraint)
    {
        if (!evaluateConstraint(ti, sc, paramscope, dedargs, fd))
            goto Lnomatch;
    }

    paramscope->pop();
    //printf("\tmatch %d\n", match);
    return (MATCH)(match | (matchTiargs<<4));

Lnomatch:
    paramscope->pop();
    //printf("\tnomatch\n");
    return MATCHnomatch;

Lerror: // todo: for the future improvement
    paramscope->pop();
    //printf("\terror\n");
    return MATCHnomatch;
}

/**************************************************
 * Declare template parameter tp with value o, and install it in the scope sc.
 */

RootObject *TemplateDeclaration::declareParameter(Scope *sc, TemplateParameter *tp, RootObject *o)
{
    //printf("TemplateDeclaration::declareParameter('%s', o = %p)\n", tp->ident->toChars(), o);

    Type *ta = isType(o);
    Expression *ea = isExpression(o);
    Dsymbol *sa = isDsymbol(o);
    Tuple *va = isTuple(o);

    Declaration *d;
    VarDeclaration *v = NULL;

    if (ea && ea->op == TOKtype)
        ta = ea->type;
    else if (ea && ea->op == TOKscope)
        sa = ((ScopeExp *)ea)->sds;
    else if (ea && (ea->op == TOKthis || ea->op == TOKsuper))
        sa = ((ThisExp *)ea)->var;
    else if (ea && ea->op == TOKfunction)
    {
        if (((FuncExp *)ea)->td)
            sa = ((FuncExp *)ea)->td;
        else
            sa = ((FuncExp *)ea)->fd;
    }

    if (ta)
    {
        //printf("type %s\n", ta->toChars());
        d = new AliasDeclaration(Loc(), tp->ident, ta);
    }
    else if (sa)
    {
        //printf("Alias %s %s;\n", sa->ident->toChars(), tp->ident->toChars());
        d = new AliasDeclaration(Loc(), tp->ident, sa);
    }
    else if (ea)
    {
        // tdtypes.data[i] always matches ea here
        Initializer *init = new ExpInitializer(loc, ea);
        TemplateValueParameter *tvp = tp->isTemplateValueParameter();

        Type *t = tvp ? tvp->valType : NULL;

        v = new VarDeclaration(loc, t, tp->ident, init);
        v->storage_class = STCmanifest | STCtemplateparameter;
        d = v;
    }
    else if (va)
    {
        //printf("\ttuple\n");
        d = new TupleDeclaration(loc, tp->ident, &va->objects);
    }
    else
    {
        assert(0);
    }

    d->storage_class |= STCtemplateparameter;
    if (ta)
    {
        Type *t = ta;
        // consistent with Type::checkDeprecated()
        while (t->ty != Tenum)
        {
            if (!t->nextOf()) break;
            t = ((TypeNext *)t)->next;
        }
        if (Dsymbol *s = t->toDsymbol(sc))
        {
            if (s->isDeprecated())
                d->storage_class |= STCdeprecated;
        }
    }
    else if (sa)
    {
        if (sa->isDeprecated())
            d->storage_class |= STCdeprecated;
    }

    if (!sc->insert(d))
        error("declaration %s is already defined", tp->ident->toChars());
    d->semantic(sc);

    /* So the caller's o gets updated with the result of semantic() being run on o
     */
    if (v)
        o = initializerToExpression(v->_init);
    return o;
}

/**************************************
 * Determine if TemplateDeclaration is variadic.
 */

TemplateTupleParameter *isVariadic(TemplateParameters *parameters)
{
    size_t dim = parameters->dim;
    TemplateTupleParameter *tp = NULL;

    if (dim)
        tp = ((*parameters)[dim - 1])->isTemplateTupleParameter();
    return tp;
}

TemplateTupleParameter *TemplateDeclaration::isVariadic()
{
    return ::isVariadic(parameters);
}

/***********************************
 * We can overload templates.
 */

bool TemplateDeclaration::isOverloadable()
{
    return true;
}

/*************************************************
 * Given function arguments, figure out which template function
 * to expand, and return matching result.
 * Input:
 *      m               matching result
 *      dstart          the root of overloaded function templates
 *      loc             instantiation location
 *      sc              instantiation scope
 *      tiargs          initial list of template arguments
 *      tthis           if !NULL, the 'this' pointer argument
 *      fargs           arguments to function
 */

void functionResolve(Match *m, Dsymbol *dstart, Loc loc, Scope *sc,
        Objects *tiargs, Type *tthis, Expressions *fargs)
{
    struct ParamDeduce
    {
        // context
        Loc loc;
        Scope *sc;
        Type *tthis;
        Objects *tiargs;
        Expressions *fargs;
        // result
        Match *m;
        int property;   // 0: unintialized
                        // 1: seen @property
                        // 2: not @property
        size_t ov_index;
        TemplateDeclaration *td_best;
        TemplateInstance *ti_best;
        MATCH ta_last;
        Type *tthis_best;

        static int fp(void *param, Dsymbol *s)
        {
            if (s->errors)
                return 0;
            if (FuncDeclaration *fd = s->isFuncDeclaration())
                return ((ParamDeduce *)param)->applyFunction(fd);
            if (TemplateDeclaration *td = s->isTemplateDeclaration())
                return ((ParamDeduce *)param)->applyTemplate(td);
            return 0;
        }

        int applyFunction(FuncDeclaration *fd)
        {
            // skip duplicates
            if (fd == m->lastf)
                return 0;
            // explicitly specified tiargs never match to non template function
            if (tiargs && tiargs->dim > 0)
                return 0;

            // constructors need a valid scope in order to detect semantic errors
            if (!fd->isCtorDeclaration() &&
                fd->semanticRun < PASSsemanticdone)
            {
                Ungag ungag = fd->ungagSpeculative();
                fd->semantic(NULL);
            }
            if (fd->semanticRun < PASSsemanticdone)
            {
                ::error(loc, "forward reference to template %s", fd->toChars());
                return 1;
            }
            //printf("fd = %s %s, fargs = %s\n", fd->toChars(), fd->type->toChars(), fargs->toChars());
            m->anyf = fd;
            TypeFunction *tf = (TypeFunction *)fd->type;

            int prop = (tf->isproperty) ? 1 : 2;
            if (property == 0)
                property = prop;
            else if (property != prop)
                error(fd->loc, "cannot overload both property and non-property functions");

            /* For constructors, qualifier check will be opposite direction.
             * Qualified constructor always makes qualified object, then will be checked
             * that it is implicitly convertible to tthis.
             */
            Type *tthis_fd = fd->needThis() ? tthis : NULL;
            bool isCtorCall = tthis_fd && fd->isCtorDeclaration();
            if (isCtorCall)
            {
                //printf("%s tf->mod = x%x tthis_fd->mod = x%x %d\n", tf->toChars(),
                //        tf->mod, tthis_fd->mod, fd->isolateReturn());
                if (MODimplicitConv(tf->mod, tthis_fd->mod) ||
                    (tf->isWild() && tf->isShared() == tthis_fd->isShared()) ||
                    fd->isolateReturn())
                {
                    /* && tf->isShared() == tthis_fd->isShared()*/
                    // Uniquely constructed object can ignore shared qualifier.
                    // TODO: Is this appropriate?
                    tthis_fd = NULL;
                }
                else
                    return 0;   // MATCHnomatch
            }
            MATCH mfa = tf->callMatch(tthis_fd, fargs);
            //printf("test1: mfa = %d\n", mfa);
            if (mfa > MATCHnomatch)
            {
                if (mfa > m->last) goto LfIsBetter;
                if (mfa < m->last) goto LlastIsBetter;

                /* See if one of the matches overrides the other.
                */
                assert(m->lastf);
                if (m->lastf->overrides(fd)) goto LlastIsBetter;
                if (fd->overrides(m->lastf)) goto LfIsBetter;

                /* Try to disambiguate using template-style partial ordering rules.
                 * In essence, if f() and g() are ambiguous, if f() can call g(),
                 * but g() cannot call f(), then pick f().
                 * This is because f() is "more specialized."
                 */
                {
                    MATCH c1 = fd->leastAsSpecialized(m->lastf);
                    MATCH c2 = m->lastf->leastAsSpecialized(fd);
                    //printf("c1 = %d, c2 = %d\n", c1, c2);
                    if (c1 > c2) goto LfIsBetter;
                    if (c1 < c2) goto LlastIsBetter;
                }

                /* The 'overrides' check above does covariant checking only
                 * for virtual member functions. It should do it for all functions,
                 * but in order to not risk breaking code we put it after
                 * the 'leastAsSpecialized' check.
                 * In the future try moving it before.
                 * I.e. a not-the-same-but-covariant match is preferred,
                 * as it is more restrictive.
                 */
                if (!m->lastf->type->equals(fd->type))
                {
                    //printf("cov: %d %d\n", m->lastf->type->covariant(fd->type), fd->type->covariant(m->lastf->type));
                    if (m->lastf->type->covariant(fd->type) == 1) goto LlastIsBetter;
                    if (fd->type->covariant(m->lastf->type) == 1) goto LfIsBetter;
                }

                /* If the two functions are the same function, like:
                 *    int foo(int);
                 *    int foo(int x) { ... }
                 * then pick the one with the body.
                 */
                if (tf->equals(m->lastf->type) &&
                    fd->storage_class == m->lastf->storage_class &&
                    fd->parent == m->lastf->parent &&
                    fd->protection == m->lastf->protection &&
                    fd->linkage == m->lastf->linkage)
                {
                    if ( fd->fbody && !m->lastf->fbody) goto LfIsBetter;
                    if (!fd->fbody &&  m->lastf->fbody) goto LlastIsBetter;
                }

                // Bugzilla 14450: Prefer exact qualified constructor for the creating object type
                if (isCtorCall && tf->mod != m->lastf->type->mod)
                {
                    if (tthis->mod == tf->mod) goto LfIsBetter;
                    if (tthis->mod == m->lastf->type->mod) goto LlastIsBetter;
                }

                m->nextf = fd;
                m->count++;
                return 0;

            LlastIsBetter:
                return 0;

            LfIsBetter:
                td_best = NULL;
                ti_best = NULL;
                ta_last = MATCHexact;
                m->last = mfa;
                m->lastf = fd;
                tthis_best = tthis_fd;
                ov_index = 0;
                m->count = 1;
                return 0;
            }
            return 0;
        }

        int applyTemplate(TemplateDeclaration *td)
        {
            //printf("applyTemplate()\n");
            // skip duplicates
            if (td == td_best)
                return 0;

            if (!sc)
                sc = td->_scope; // workaround for Type::aliasthisOf

            if (td->semanticRun == PASSinit && td->_scope)
            {
                // Try to fix forward reference. Ungag errors while doing so.
                Ungag ungag = td->ungagSpeculative();
                td->semantic(td->_scope);
            }
            if (td->semanticRun == PASSinit)
            {
                ::error(loc, "forward reference to template %s", td->toChars());
            Lerror:
                m->lastf = NULL;
                m->count = 0;
                m->last = MATCHnomatch;
                return 1;
            }
            //printf("td = %s\n", td->toChars());

            FuncDeclaration *f;
            f = td->onemember ? td->onemember->isFuncDeclaration() : NULL;
            if (!f)
            {
                if (!tiargs)
                    tiargs = new Objects();
                TemplateInstance *ti = new TemplateInstance(loc, td, tiargs);
                Objects dedtypes;
                dedtypes.setDim(td->parameters->dim);
                assert(td->semanticRun != PASSinit);
                MATCH mta = td->matchWithInstance(sc, ti, &dedtypes, fargs, 0);
                //printf("matchWithInstance = %d\n", mta);
                if (mta <= MATCHnomatch || mta < ta_last)      // no match or less match
                    return 0;

                ti->semantic(sc, fargs);
                if (!ti->inst)                  // if template failed to expand
                    return 0;

                Dsymbol *s = ti->inst->toAlias();
                FuncDeclaration *fd;
                if (TemplateDeclaration *tdx = s->isTemplateDeclaration())
                {
                    Objects dedtypesX;  // empty tiargs

                    // Bugzilla 11553: Check for recursive instantiation of tdx.
                    for (TemplatePrevious *p = tdx->previous; p; p = p->prev)
                    {
                        if (arrayObjectMatch(p->dedargs, &dedtypesX))
                        {
                            //printf("recursive, no match p->sc=%p %p %s\n", p->sc, this, this->toChars());
                            /* It must be a subscope of p->sc, other scope chains are not recursive
                             * instantiations.
                             */
                            for (Scope *scx = sc; scx; scx = scx->enclosing)
                            {
                                if (scx == p->sc)
                                {
                                    error(loc, "recursive template expansion while looking for %s.%s", ti->toChars(), tdx->toChars());
                                    goto Lerror;
                                }
                            }
                        }
                        /* BUG: should also check for ref param differences
                        */
                    }

                    TemplatePrevious pr;
                    pr.prev = tdx->previous;
                    pr.sc = sc;
                    pr.dedargs = &dedtypesX;
                    tdx->previous = &pr;                 // add this to threaded list

                    fd = resolveFuncCall(loc, sc, s, NULL, tthis, fargs, 1);

                    tdx->previous = pr.prev;             // unlink from threaded list
                }
                else if (s->isFuncDeclaration())
                {
                    fd = resolveFuncCall(loc, sc, s, NULL, tthis, fargs, 1);
                }
                else
                    goto Lerror;

                if (!fd)
                    return 0;

                if (fd->type->ty != Tfunction)
                {
                    m->lastf = fd;   // to propagate "error match"
                    m->count = 1;
                    m->last = MATCHnomatch;
                    return 1;
                }

                Type *tthis_fd = fd->needThis() && !fd->isCtorDeclaration() ? tthis : NULL;

                TypeFunction *tf = (TypeFunction *)fd->type;
                MATCH mfa = tf->callMatch(tthis_fd, fargs);
                if (mfa < m->last)
                    return 0;

                if (mta < ta_last) goto Ltd_best2;
                if (mta > ta_last) goto Ltd2;

                if (mfa < m->last) goto Ltd_best2;
                if (mfa > m->last) goto Ltd2;

                //printf("Lambig2\n");
                m->nextf = fd;
                m->count++;
                return 0;

            Ltd_best2:
                return 0;

            Ltd2:
                // td is the new best match
                assert(td->_scope);
                td_best = td;
                ti_best = NULL;
                property = 0;   // (backward compatibility)
                ta_last = mta;
                m->last = mfa;
                m->lastf = fd;
                tthis_best = tthis_fd;
                ov_index = 0;
                m->nextf = NULL;
                m->count = 1;
                return 0;
            }

            //printf("td = %s\n", td->toChars());
            for (size_t ovi = 0; f; f = f->overnext0, ovi++)
            {
                if (f->type->ty != Tfunction || f->errors)
                    goto Lerror;

                /* This is a 'dummy' instance to evaluate constraint properly.
                */
                TemplateInstance *ti = new TemplateInstance(loc, td, tiargs);
                ti->parent = td->parent;    // Maybe calculating valid 'enclosing' is unnecessary.

                FuncDeclaration *fd = f;
                int x = td->deduceFunctionTemplateMatch(ti, sc, fd, tthis, fargs);
                MATCH mta = (MATCH)(x >> 4);
                MATCH mfa = (MATCH)(x & 0xF);
                //printf("match:t/f = %d/%d\n", mta, mfa);
                if (!fd || mfa == MATCHnomatch)
                    continue;

                Type *tthis_fd = fd->needThis() ? tthis : NULL;

                bool isCtorCall = tthis_fd && fd->isCtorDeclaration();
                if (isCtorCall)
                {
                    // Constructor call requires additional check.

                    TypeFunction *tf = (TypeFunction *)fd->type;
                    assert(tf->next);
                    if (MODimplicitConv(tf->mod, tthis_fd->mod) ||
                        (tf->isWild() && tf->isShared() == tthis_fd->isShared()) ||
                        fd->isolateReturn())
                    {
                        tthis_fd = NULL;
                    }
                    else
                        continue;   // MATCHnomatch
                }

                if (mta < ta_last) goto Ltd_best;
                if (mta > ta_last) goto Ltd;

                if (mfa < m->last) goto Ltd_best;
                if (mfa > m->last) goto Ltd;

                if (td_best)
                {
                    // Disambiguate by picking the most specialized TemplateDeclaration
                    MATCH c1 = td->leastAsSpecialized(sc, td_best, fargs);
                    MATCH c2 = td_best->leastAsSpecialized(sc, td, fargs);
                    //printf("1: c1 = %d, c2 = %d\n", c1, c2);
                    if (c1 > c2) goto Ltd;
                    if (c1 < c2) goto Ltd_best;
                }
                assert(fd && m->lastf);
                {
                    // Disambiguate by tf->callMatch
                    TypeFunction *tf1 = (TypeFunction *)fd->type;
                    assert(tf1->ty == Tfunction);
                    TypeFunction *tf2 = (TypeFunction *)m->lastf->type;
                    assert(tf2->ty == Tfunction);
                    MATCH c1 = tf1->callMatch(tthis_fd,   fargs);
                    MATCH c2 = tf2->callMatch(tthis_best, fargs);
                    //printf("2: c1 = %d, c2 = %d\n", c1, c2);
                    if (c1 > c2) goto Ltd;
                    if (c1 < c2) goto Ltd_best;
                }
                {
                    // Disambiguate by picking the most specialized FunctionDeclaration
                    MATCH c1 = fd->leastAsSpecialized(m->lastf);
                    MATCH c2 = m->lastf->leastAsSpecialized(fd);
                    //printf("3: c1 = %d, c2 = %d\n", c1, c2);
                    if (c1 > c2) goto Ltd;
                    if (c1 < c2) goto Ltd_best;
                }

                // Bugzilla 14450: Prefer exact qualified constructor for the creating object type
                if (isCtorCall && fd->type->mod != m->lastf->type->mod)
                {
                    if (tthis->mod == fd->type->mod) goto Ltd;
                    if (tthis->mod == m->lastf->type->mod) goto Ltd_best;
                }

                m->nextf = fd;
                m->count++;
                continue;

            Ltd_best:         // td_best is the best match so far
                //printf("Ltd_best\n");
                continue;

            Ltd:              // td is the new best match
                //printf("Ltd\n");
                assert(td->_scope);
                td_best = td;
                ti_best = ti;
                property = 0;   // (backward compatibility)
                ta_last = mta;
                m->last = mfa;
                m->lastf = fd;
                tthis_best = tthis_fd;
                ov_index = ovi;
                m->nextf = NULL;
                m->count = 1;
                continue;
            }
            return 0;
        }
    };
    ParamDeduce p;
    // context
    p.loc    = loc;
    p.sc     = sc;
    p.tthis  = tthis;
    p.tiargs = tiargs;
    p.fargs  = fargs;

    // result
    p.m          = m;
    p.property   = 0;
    p.ov_index   = 0;
    p.td_best    = NULL;
    p.ti_best    = NULL;
    p.ta_last    = m->last != MATCHnomatch ? MATCHexact : MATCHnomatch;
    p.tthis_best = NULL;

    TemplateDeclaration *td = dstart->isTemplateDeclaration();
    if (td && td->funcroot)
        dstart = td->funcroot;

    overloadApply(dstart, &p, &ParamDeduce::fp);

    //printf("td_best = %p, m->lastf = %p\n", p.td_best, m->lastf);
    if (p.td_best && p.ti_best && m->count == 1)
    {
        // Matches to template function
        assert(p.td_best->onemember && p.td_best->onemember->isFuncDeclaration());

        /* The best match is td_best with arguments tdargs.
         * Now instantiate the template.
         */
        assert(p.td_best->_scope);
        if (!sc)
            sc = p.td_best->_scope; // workaround for Type::aliasthisOf

        TemplateInstance *ti = new TemplateInstance(loc, p.td_best, p.ti_best->tiargs);
        ti->semantic(sc, fargs);

        m->lastf = ti->toAlias()->isFuncDeclaration();
        if (!m->lastf)
            goto Lnomatch;
        if (ti->errors)
        {
        Lerror:
            m->count = 1;
            assert(m->lastf);
            m->last = MATCHnomatch;
            return;
        }

        // look forward instantiated overload function
        // Dsymbol::oneMembers is alredy called in TemplateInstance::semantic.
        // it has filled overnext0d
        while (p.ov_index--)
        {
            m->lastf = m->lastf->overnext0;
            assert(m->lastf);
        }

        p.tthis_best = m->lastf->needThis() && !m->lastf->isCtorDeclaration() ? tthis : NULL;

        TypeFunction *tf = (TypeFunction *)m->lastf->type;
        if (tf->ty == Terror)
            goto Lerror;
        assert(tf->ty == Tfunction);
        if (!tf->callMatch(p.tthis_best, fargs))
            goto Lnomatch;

        /* As Bugzilla 3682 shows, a template instance can be matched while instantiating
         * that same template. Thus, the function type can be incomplete. Complete it.
         *
         * Bugzilla 9208: For auto function, completion should be deferred to the end of
         * its semantic3. Should not complete it in here.
         */
        if (tf->next && !m->lastf->inferRetType)
        {
            m->lastf->type = tf->semantic(loc, sc);
        }
    }
    else if (m->lastf)
    {
        // Matches to non template function,
        // or found matches were ambiguous.
        assert(m->count >= 1);
    }
    else
    {
    Lnomatch:
        m->count = 0;
        m->lastf = NULL;
        m->last = MATCHnomatch;
    }
}

/*************************************************
 * Limited function template instantiation for using fd->leastAsSpecialized()
 */
FuncDeclaration *TemplateDeclaration::doHeaderInstantiation(
        TemplateInstance *ti, Scope *sc2,
        FuncDeclaration *fd, Type *tthis, Expressions *fargs)
{
    assert(fd);

    // function body and contracts are not need
    if (fd->isCtorDeclaration())
        fd = new CtorDeclaration(fd->loc, fd->endloc, fd->storage_class, fd->type->syntaxCopy());
    else
        fd = new FuncDeclaration(fd->loc, fd->endloc, fd->ident, fd->storage_class, fd->type->syntaxCopy());
    fd->parent = ti;

    assert(fd->type->ty == Tfunction);
    TypeFunction *tf = (TypeFunction *)fd->type;
    tf->fargs = fargs;

    if (tthis)
    {
        // Match 'tthis' to any TemplateThisParameter's
        bool hasttp = false;
        for (size_t i = 0; i < parameters->dim; i++)
        {
            TemplateParameter *tp = (*parameters)[i];
            TemplateThisParameter *ttp = tp->isTemplateThisParameter();
            if (ttp)
                hasttp = true;
        }
        if (hasttp)
        {
            tf = (TypeFunction *)tf->addSTC(ModToStc(tthis->mod));
            assert(!tf->deco);
        }
    }

    Scope *scx = sc2->push();

    // Shouldn't run semantic on default arguments and return type.
    for (size_t i = 0; i < tf->parameters->dim; i++)
        (*tf->parameters)[i]->defaultArg = NULL;
    if (fd->isCtorDeclaration())
    {
        // For constructors, emitting return type is necessary for
        // isolateReturn() in functionResolve.
        scx->flags |= SCOPEctor;

        Dsymbol *parent = toParent2();
        Type *tret;
        AggregateDeclaration *ad = parent->isAggregateDeclaration();
        if (!ad || parent->isUnionDeclaration())
        {
            tret = Type::tvoid;
        }
        else
        {
            tret = ad->handleType();
            assert(tret);
            tret = tret->addStorageClass(fd->storage_class | scx->stc);
            tret = tret->addMod(tf->mod);
        }
        tf->next = tret;
        if (ad && ad->isStructDeclaration())
            tf->isref = 1;
        //printf("tf = %s\n", tf->toChars());
    }
    else
        tf->next = NULL;
    fd->type = tf;
    fd->type = fd->type->addSTC(scx->stc);
    fd->type = fd->type->semantic(fd->loc, scx);
    scx = scx->pop();

    if (fd->type->ty != Tfunction)
        return NULL;

    fd->originalType = fd->type;    // for mangling
    //printf("\t[%s] fd->type = %s, mod = %x, ", loc.toChars(), fd->type->toChars(), fd->type->mod);
    //printf("fd->needThis() = %d\n", fd->needThis());

    return fd;
}

bool TemplateDeclaration::hasStaticCtorOrDtor()
{
    return false;               // don't scan uninstantiated templates
}

const char *TemplateDeclaration::toChars()
{
    if (literal)
        return Dsymbol::toChars();

    OutBuffer buf;
    HdrGenState hgs;

    buf.writestring(ident->toChars());
    buf.writeByte('(');
    for (size_t i = 0; i < parameters->dim; i++)
    {
        TemplateParameter *tp = (*parameters)[i];
        if (i)
            buf.writestring(", ");
        ::toCBuffer(tp, &buf, &hgs);
    }
    buf.writeByte(')');

    if (onemember)
    {
        FuncDeclaration *fd = onemember->isFuncDeclaration();
        if (fd && fd->type)
        {
            TypeFunction *tf = (TypeFunction *)fd->type;
            buf.writestring(parametersTypeToChars(tf->parameters, tf->varargs));
        }
    }

    if (constraint)
    {
        buf.writestring(" if (");
        ::toCBuffer(constraint, &buf, &hgs);
        buf.writeByte(')');
    }
    return buf.extractString();
}

Prot TemplateDeclaration::prot()
{
    return protection;
}

/****************************************************
 * Given a new instance tithis of this TemplateDeclaration,
 * see if there already exists an instance.
 * If so, return that existing instance.
 */

TemplateInstance *TemplateDeclaration::findExistingInstance(TemplateInstance *tithis, Expressions *fargs)
{
    //printf("findExistingInstance(%p)\n", tithis);
    tithis->fargs = fargs;
    TemplateInstances *tinstances = (TemplateInstances *)dmd_aaGetRvalue((AA *)instances, (void *)tithis->toHash());
    if (tinstances)
    {
        for (size_t i = 0; i < tinstances->dim; i++)
        {
            TemplateInstance *ti = (*tinstances)[i];
            if (tithis->compare(ti) == 0)
                return ti;
        }
    }
    return NULL;
}

/********************************************
 * Add instance ti to TemplateDeclaration's table of instances.
 * Return a handle we can use to later remove it if it fails instantiation.
 */

TemplateInstance *TemplateDeclaration::addInstance(TemplateInstance *ti)
{
    //printf("addInstance() %p %p\n", instances, ti);
    TemplateInstances **ptinstances = (TemplateInstances **)dmd_aaGet((AA **)&instances, (void *)ti->toHash());
    if (!*ptinstances)
        *ptinstances = new TemplateInstances();
    (*ptinstances)->push(ti);
    return ti;
}

/*******************************************
 * Remove TemplateInstance from table of instances.
 * Input:
 *      handle returned by addInstance()
 */

void TemplateDeclaration::removeInstance(TemplateInstance *handle)
{
    //printf("removeInstance()\n");
    TemplateInstances *tinstances = (TemplateInstances *)dmd_aaGetRvalue((AA *)instances, (void *)handle->toHash());
    if (tinstances)
    {
        for (size_t i = 0; i < tinstances->dim; i++)
        {
            TemplateInstance *ti = (*tinstances)[i];
            if (handle == ti)
            {
                tinstances->remove(i);
                break;
            }
        }
    }
}

/* ======================== Type ============================================ */

/****
 * Given an identifier, figure out which TemplateParameter it is.
 * Return IDX_NOTFOUND if not found.
 */

static size_t templateIdentifierLookup(Identifier *id, TemplateParameters *parameters)
{
    for (size_t i = 0; i < parameters->dim; i++)
    {
        TemplateParameter *tp = (*parameters)[i];
        if (tp->ident->equals(id))
            return i;
    }
    return IDX_NOTFOUND;
}

size_t templateParameterLookup(Type *tparam, TemplateParameters *parameters)
{
    if (tparam->ty == Tident)
    {
        TypeIdentifier *tident = (TypeIdentifier *)tparam;
        //printf("\ttident = '%s'\n", tident->toChars());
        return templateIdentifierLookup(tident->ident, parameters);
    }
    return IDX_NOTFOUND;
}

unsigned char deduceWildHelper(Type *t, Type **at, Type *tparam)
{
    if ((tparam->mod & MODwild) == 0)
        return 0;

    *at = NULL;

    #define X(U,T)  ((U) << 4) | (T)
    switch (X(tparam->mod, t->mod))
    {
        case X(MODwild,                     0):
        case X(MODwild,                     MODconst):
        case X(MODwild,                     MODshared):
        case X(MODwild,                     MODshared | MODconst):
        case X(MODwild,                     MODimmutable):
        case X(MODwildconst,                0):
        case X(MODwildconst,                MODconst):
        case X(MODwildconst,                MODshared):
        case X(MODwildconst,                MODshared | MODconst):
        case X(MODwildconst,                MODimmutable):
        case X(MODshared | MODwild,         MODshared):
        case X(MODshared | MODwild,         MODshared | MODconst):
        case X(MODshared | MODwild,         MODimmutable):
        case X(MODshared | MODwildconst,    MODshared):
        case X(MODshared | MODwildconst,    MODshared | MODconst):
        case X(MODshared | MODwildconst,    MODimmutable):
        {
            unsigned char wm = (t->mod & ~MODshared);
            if (wm == 0)
                wm = MODmutable;
            unsigned char m = (t->mod & (MODconst | MODimmutable)) | (tparam->mod & t->mod & MODshared);
            *at = t->unqualify(m);
            return wm;
        }

        case X(MODwild,                     MODwild):
        case X(MODwild,                     MODwildconst):
        case X(MODwild,                     MODshared | MODwild):
        case X(MODwild,                     MODshared | MODwildconst):
        case X(MODwildconst,                MODwild):
        case X(MODwildconst,                MODwildconst):
        case X(MODwildconst,                MODshared | MODwild):
        case X(MODwildconst,                MODshared | MODwildconst):
        case X(MODshared | MODwild,         MODshared | MODwild):
        case X(MODshared | MODwild,         MODshared | MODwildconst):
        case X(MODshared | MODwildconst,    MODshared | MODwild):
        case X(MODshared | MODwildconst,    MODshared | MODwildconst):
        {
            *at = t->unqualify(tparam->mod & t->mod);
            return MODwild;
        }

        default:
            return 0;
    }
    #undef X
}

MATCH deduceTypeHelper(Type *t, Type **at, Type *tparam)
{
    // 9*9 == 81 cases

    #define X(U,T)  ((U) << 4) | (T)
    switch (X(tparam->mod, t->mod))
    {
        case X(0, 0):
        case X(0, MODconst):
        case X(0, MODwild):
        case X(0, MODwildconst):
        case X(0, MODshared):
        case X(0, MODshared | MODconst):
        case X(0, MODshared | MODwild):
        case X(0, MODshared | MODwildconst):
        case X(0, MODimmutable):
            // foo(U)                       T                       => T
            // foo(U)                       const(T)                => const(T)
            // foo(U)                       inout(T)                => inout(T)
            // foo(U)                       inout(const(T))         => inout(const(T))
            // foo(U)                       shared(T)               => shared(T)
            // foo(U)                       shared(const(T))        => shared(const(T))
            // foo(U)                       shared(inout(T))        => shared(inout(T))
            // foo(U)                       shared(inout(const(T))) => shared(inout(const(T)))
            // foo(U)                       immutable(T)            => immutable(T)
        {
            *at = t;
            return MATCHexact;
        }

        case X(MODconst,                    MODconst):
        case X(MODwild,                     MODwild):
        case X(MODwildconst,                MODwildconst):
        case X(MODshared,                   MODshared):
        case X(MODshared | MODconst,        MODshared | MODconst):
        case X(MODshared | MODwild,         MODshared | MODwild):
        case X(MODshared | MODwildconst,    MODshared | MODwildconst):
        case X(MODimmutable,                MODimmutable):
            // foo(const(U))                const(T)                => T
            // foo(inout(U))                inout(T)                => T
            // foo(inout(const(U)))         inout(const(T))         => T
            // foo(shared(U))               shared(T)               => T
            // foo(shared(const(U)))        shared(const(T))        => T
            // foo(shared(inout(U)))        shared(inout(T))        => T
            // foo(shared(inout(const(U)))) shared(inout(const(T))) => T
            // foo(immutable(U))            immutable(T)            => T
        {
            *at = t->mutableOf()->unSharedOf();
            return MATCHexact;
        }

        case X(MODconst,                    0):
        case X(MODconst,                    MODwild):
        case X(MODconst,                    MODwildconst):
        case X(MODconst,                    MODshared | MODconst):
        case X(MODconst,                    MODshared | MODwild):
        case X(MODconst,                    MODshared | MODwildconst):
        case X(MODconst,                    MODimmutable):
        case X(MODwild,                     MODshared | MODwild):
        case X(MODwildconst,                MODshared | MODwildconst):
        case X(MODshared | MODconst,        MODimmutable):
            // foo(const(U))                T                       => T
            // foo(const(U))                inout(T)                => T
            // foo(const(U))                inout(const(T))         => T
            // foo(const(U))                shared(const(T))        => shared(T)
            // foo(const(U))                shared(inout(T))        => shared(T)
            // foo(const(U))                shared(inout(const(T))) => shared(T)
            // foo(const(U))                immutable(T)            => T
            // foo(inout(U))                shared(inout(T))        => shared(T)
            // foo(inout(const(U)))         shared(inout(const(T))) => shared(T)
            // foo(shared(const(U)))        immutable(T)            => T
        {
            *at = t->mutableOf();
            return MATCHconst;
        }

        case X(MODconst,                    MODshared):
            // foo(const(U))                shared(T)               => shared(T)
        {
            *at = t;
            return MATCHconst;
        }

        case X(MODshared,                   MODshared | MODconst):
        case X(MODshared,                   MODshared | MODwild):
        case X(MODshared,                   MODshared | MODwildconst):
        case X(MODshared | MODconst,        MODshared):
            // foo(shared(U))               shared(const(T))        => const(T)
            // foo(shared(U))               shared(inout(T))        => inout(T)
            // foo(shared(U))               shared(inout(const(T))) => inout(const(T))
            // foo(shared(const(U)))        shared(T)               => T
        {
            *at = t->unSharedOf();
            return MATCHconst;
        }

        case X(MODwildconst,                MODimmutable):
        case X(MODshared | MODconst,        MODshared | MODwildconst):
        case X(MODshared | MODwildconst,    MODimmutable):
        case X(MODshared | MODwildconst,    MODshared | MODwild):
            // foo(inout(const(U)))         immutable(T)            => T
            // foo(shared(const(U)))        shared(inout(const(T))) => T
            // foo(shared(inout(const(U)))) immutable(T)            => T
            // foo(shared(inout(const(U)))) shared(inout(T))        => T
        {
            *at = t->unSharedOf()->mutableOf();
            return MATCHconst;
        }

        case X(MODshared | MODconst,        MODshared | MODwild):
            // foo(shared(const(U)))        shared(inout(T))        => T
        {
            *at = t->unSharedOf()->mutableOf();
            return MATCHconst;
        }

        case X(MODwild,                     0):
        case X(MODwild,                     MODconst):
        case X(MODwild,                     MODwildconst):
        case X(MODwild,                     MODimmutable):
        case X(MODwild,                     MODshared):
        case X(MODwild,                     MODshared | MODconst):
        case X(MODwild,                     MODshared | MODwildconst):
        case X(MODwildconst,                0):
        case X(MODwildconst,                MODconst):
        case X(MODwildconst,                MODwild):
        case X(MODwildconst,                MODshared):
        case X(MODwildconst,                MODshared | MODconst):
        case X(MODwildconst,                MODshared | MODwild):
        case X(MODshared,                   0):
        case X(MODshared,                   MODconst):
        case X(MODshared,                   MODwild):
        case X(MODshared,                   MODwildconst):
        case X(MODshared,                   MODimmutable):
        case X(MODshared | MODconst,        0):
        case X(MODshared | MODconst,        MODconst):
        case X(MODshared | MODconst,        MODwild):
        case X(MODshared | MODconst,        MODwildconst):
        case X(MODshared | MODwild,         0):
        case X(MODshared | MODwild,         MODconst):
        case X(MODshared | MODwild,         MODwild):
        case X(MODshared | MODwild,         MODwildconst):
        case X(MODshared | MODwild,         MODimmutable):
        case X(MODshared | MODwild,         MODshared):
        case X(MODshared | MODwild,         MODshared | MODconst):
        case X(MODshared | MODwild,         MODshared | MODwildconst):
        case X(MODshared | MODwildconst,    0):
        case X(MODshared | MODwildconst,    MODconst):
        case X(MODshared | MODwildconst,    MODwild):
        case X(MODshared | MODwildconst,    MODwildconst):
        case X(MODshared | MODwildconst,    MODshared):
        case X(MODshared | MODwildconst,    MODshared | MODconst):
        case X(MODimmutable,                0):
        case X(MODimmutable,                MODconst):
        case X(MODimmutable,                MODwild):
        case X(MODimmutable,                MODwildconst):
        case X(MODimmutable,                MODshared):
        case X(MODimmutable,                MODshared | MODconst):
        case X(MODimmutable,                MODshared | MODwild):
        case X(MODimmutable,                MODshared | MODwildconst):
            // foo(inout(U))                T                       => nomatch
            // foo(inout(U))                const(T)                => nomatch
            // foo(inout(U))                inout(const(T))         => nomatch
            // foo(inout(U))                immutable(T)            => nomatch
            // foo(inout(U))                shared(T)               => nomatch
            // foo(inout(U))                shared(const(T))        => nomatch
            // foo(inout(U))                shared(inout(const(T))) => nomatch
            // foo(inout(const(U)))         T                       => nomatch
            // foo(inout(const(U)))         const(T)                => nomatch
            // foo(inout(const(U)))         inout(T)                => nomatch
            // foo(inout(const(U)))         shared(T)               => nomatch
            // foo(inout(const(U)))         shared(const(T))        => nomatch
            // foo(inout(const(U)))         shared(inout(T))        => nomatch
            // foo(shared(U))               T                       => nomatch
            // foo(shared(U))               const(T)                => nomatch
            // foo(shared(U))               inout(T)                => nomatch
            // foo(shared(U))               inout(const(T))         => nomatch
            // foo(shared(U))               immutable(T)            => nomatch
            // foo(shared(const(U)))        T                       => nomatch
            // foo(shared(const(U)))        const(T)                => nomatch
            // foo(shared(const(U)))        inout(T)                => nomatch
            // foo(shared(const(U)))        inout(const(T))         => nomatch
            // foo(shared(inout(U)))        T                       => nomatch
            // foo(shared(inout(U)))        const(T)                => nomatch
            // foo(shared(inout(U)))        inout(T)                => nomatch
            // foo(shared(inout(U)))        inout(const(T))         => nomatch
            // foo(shared(inout(U)))        immutable(T)            => nomatch
            // foo(shared(inout(U)))        shared(T)               => nomatch
            // foo(shared(inout(U)))        shared(const(T))        => nomatch
            // foo(shared(inout(U)))        shared(inout(const(T))) => nomatch
            // foo(shared(inout(const(U)))) T                       => nomatch
            // foo(shared(inout(const(U)))) const(T)                => nomatch
            // foo(shared(inout(const(U)))) inout(T)                => nomatch
            // foo(shared(inout(const(U)))) inout(const(T))         => nomatch
            // foo(shared(inout(const(U)))) shared(T)               => nomatch
            // foo(shared(inout(const(U)))) shared(const(T))        => nomatch
            // foo(immutable(U))            T                       => nomatch
            // foo(immutable(U))            const(T)                => nomatch
            // foo(immutable(U))            inout(T)                => nomatch
            // foo(immutable(U))            inout(const(T))         => nomatch
            // foo(immutable(U))            shared(T)               => nomatch
            // foo(immutable(U))            shared(const(T))        => nomatch
            // foo(immutable(U))            shared(inout(T))        => nomatch
            // foo(immutable(U))            shared(inout(const(T))) => nomatch
            return MATCHnomatch;

        default:
            assert(0);
            return MATCHnomatch; // silence compiler warning about missing return
    }
    #undef X
}

/* These form the heart of template argument deduction.
 * Given 'this' being the type argument to the template instance,
 * it is matched against the template declaration parameter specialization
 * 'tparam' to determine the type to be used for the parameter.
 * Example:
 *      template Foo(T:T*)      // template declaration
 *      Foo!(int*)              // template instantiation
 * Input:
 *      this = int*
 *      tparam = T*
 *      parameters = [ T:T* ]   // Array of TemplateParameter's
 * Output:
 *      dedtypes = [ int ]      // Array of Expression/Type's
 */
MATCH deduceType(RootObject *o, Scope *sc, Type *tparam, TemplateParameters *parameters,
        Objects *dedtypes, unsigned *wm, size_t inferStart)
{
    class DeduceType : public Visitor
    {
    public:
        Scope *sc;
        Type *tparam;
        TemplateParameters *parameters;
        Objects *dedtypes;
        unsigned *wm;
        size_t inferStart;
        MATCH result;

        DeduceType(Scope *sc, Type *tparam, TemplateParameters *parameters, Objects *dedtypes, unsigned *wm, size_t inferStart)
            : sc(sc), tparam(tparam), parameters(parameters), dedtypes(dedtypes), wm(wm), inferStart(inferStart)
        {
            result = MATCHnomatch;
        }

        void visit(Type *t)
        {
            if (!tparam)
                goto Lnomatch;

            if (t == tparam)
                goto Lexact;

            if (tparam->ty == Tident)
            {
                // Determine which parameter tparam is
                size_t i = templateParameterLookup(tparam, parameters);
                if (i == IDX_NOTFOUND)
                {
                    if (!sc)
                        goto Lnomatch;

                    /* Need a loc to go with the semantic routine.
                     */
                    Loc loc;
                    if (parameters->dim)
                    {
                        TemplateParameter *tp = (*parameters)[0];
                        loc = tp->loc;
                    }

                    /* BUG: what if tparam is a template instance, that
                     * has as an argument another Tident?
                     */
                    tparam = tparam->semantic(loc, sc);
                    assert(tparam->ty != Tident);
                    result = deduceType(t, sc, tparam, parameters, dedtypes, wm);
                    return;
                }

                TemplateParameter *tp = (*parameters)[i];

                TypeIdentifier *tident = (TypeIdentifier *)tparam;
                if (tident->idents.dim > 0)
                {
                    //printf("matching %s to %s\n", tparam->toChars(), t->toChars());
                    Dsymbol *s = t->toDsymbol(sc);
                    for (size_t j = tident->idents.dim; j-- > 0; )
                    {
                        RootObject *id = tident->idents[j];
                        if (id->dyncast() == DYNCAST_IDENTIFIER)
                        {
                            if (!s || !s->parent)
                                goto Lnomatch;
                            Dsymbol *s2 = s->parent->search(Loc(), (Identifier *)id);
                            if (!s2)
                                goto Lnomatch;
                            s2 = s2->toAlias();
                            //printf("[%d] s = %s %s, s2 = %s %s\n", j, s->kind(), s->toChars(), s2->kind(), s2->toChars());
                            if (s != s2)
                            {
                                if (Type *tx = s2->getType())
                                {
                                    if (s != tx->toDsymbol(sc))
                                        goto Lnomatch;
                                }
                                else
                                    goto Lnomatch;
                            }
                            s = s->parent;
                        }
                        else
                            goto Lnomatch;
                    }
                    //printf("[e] s = %s\n", s?s->toChars():"(null)");
                    if (tp->isTemplateTypeParameter())
                    {
                        Type *tt = s->getType();
                        if (!tt)
                            goto Lnomatch;
                        Type *at = (Type *)(*dedtypes)[i];
                        if (at && at->ty == Tnone)
                            at = ((TypeDeduced *)at)->tded;
                        if (!at || tt->equals(at))
                        {
                            (*dedtypes)[i] = tt;
                            goto Lexact;
                        }
                    }
                    if (tp->isTemplateAliasParameter())
                    {
                        Dsymbol *s2 = (Dsymbol *)(*dedtypes)[i];
                        if (!s2 || s == s2)
                        {
                            (*dedtypes)[i] = s;
                            goto Lexact;
                        }
                    }
                    goto Lnomatch;
                }

                // Found the corresponding parameter tp
                if (!tp->isTemplateTypeParameter())
                    goto Lnomatch;

                Type *at = (Type *)(*dedtypes)[i];
                Type *tt;
                if (unsigned char wx = wm ? deduceWildHelper(t, &tt, tparam) : 0)
                {
                    // type vs (none)
                    if (!at)
                    {
                        (*dedtypes)[i] = tt;
                        *wm |= wx;
                        result = MATCHconst;
                        return;
                    }

                    // type vs expressions
                    if (at->ty == Tnone)
                    {
                        TypeDeduced *xt = (TypeDeduced *)at;
                        result = xt->matchAll(tt);
                        if (result > MATCHnomatch)
                        {
                            (*dedtypes)[i] = tt;
                            if (result > MATCHconst)
                                result = MATCHconst;    // limit level for inout matches
                            delete xt;
                        }
                        return;
                    }

                    // type vs type
                    if (tt->equals(at))
                    {
                        (*dedtypes)[i] = tt;    // Prefer current type match
                        goto Lconst;
                    }
                    if (tt->implicitConvTo(at->constOf()))
                    {
                        (*dedtypes)[i] = at->constOf()->mutableOf();
                        *wm |= MODconst;
                        goto Lconst;
                    }
                    if (at->implicitConvTo(tt->constOf()))
                    {
                        (*dedtypes)[i] = tt->constOf()->mutableOf();
                        *wm |= MODconst;
                        goto Lconst;
                    }
                    goto Lnomatch;
                }
                else if (MATCH m = deduceTypeHelper(t, &tt, tparam))
                {
                    // type vs (none)
                    if (!at)
                    {
                        (*dedtypes)[i] = tt;
                        result = m;
                        return;
                    }

                    // type vs expressions
                    if (at->ty == Tnone)
                    {
                        TypeDeduced *xt = (TypeDeduced *)at;
                        result = xt->matchAll(tt);
                        if (result > MATCHnomatch)
                        {
                            (*dedtypes)[i] = tt;
                            delete xt;
                        }
                        return;
                    }

                    // type vs type
                    if (tt->equals(at))
                    {
                        goto Lexact;
                    }
                    if (tt->ty == Tclass && at->ty == Tclass)
                    {
                        result = tt->implicitConvTo(at);
                        return;
                    }
                    if (tt->ty == Tsarray && at->ty == Tarray &&
                        tt->nextOf()->implicitConvTo(at->nextOf()) >= MATCHconst)
                    {
                        goto Lexact;
                    }
                }
                goto Lnomatch;
            }

            if (tparam->ty == Ttypeof)
            {
                /* Need a loc to go with the semantic routine.
                 */
                Loc loc;
                if (parameters->dim)
                {
                    TemplateParameter *tp = (*parameters)[0];
                    loc = tp->loc;
                }

                tparam = tparam->semantic(loc, sc);
            }
            if (t->ty != tparam->ty)
            {
                if (Dsymbol *sym = t->toDsymbol(sc))
                {
                    if (sym->isforwardRef() && !tparam->deco)
                        goto Lnomatch;
                }

                MATCH m = t->implicitConvTo(tparam);
                if (m == MATCHnomatch)
                {
                    if (t->ty == Tclass)
                    {
                        TypeClass *tc = (TypeClass *)t;
                        if (tc->sym->aliasthis && !(tc->att & RECtracingDT))
                        {
                            tc->att = (AliasThisRec)(tc->att | RECtracingDT);
                            m = deduceType(t->aliasthisOf(), sc, tparam, parameters, dedtypes, wm);
                            tc->att = (AliasThisRec)(tc->att & ~RECtracingDT);
                        }
                    }
                    else if (t->ty == Tstruct)
                    {
                        TypeStruct *ts = (TypeStruct *)t;
                        if (ts->sym->aliasthis && !(ts->att & RECtracingDT))
                        {
                            ts->att = (AliasThisRec)(ts->att | RECtracingDT);
                            m = deduceType(t->aliasthisOf(), sc, tparam, parameters, dedtypes, wm);
                            ts->att = (AliasThisRec)(ts->att & ~RECtracingDT);
                        }
                    }
                }
                result = m;
                return;
            }

            if (t->nextOf())
            {
                if (tparam->deco && !tparam->hasWild())
                {
                    result = t->implicitConvTo(tparam);
                    return;
                }

                Type *tpn = tparam->nextOf();
                if (wm && t->ty == Taarray && tparam->isWild())
                {
                    // Bugzilla 12403: In IFTI, stop inout matching on transitive part of AA types.
                    tpn = tpn->substWildTo(MODmutable);
                }

                result = deduceType(t->nextOf(), sc, tpn, parameters, dedtypes, wm);
                return;
            }

        Lexact:
            result = MATCHexact;
            return;

        Lnomatch:
            result = MATCHnomatch;
            return;

        Lconst:
            result = MATCHconst;
        }

        void visit(TypeVector *t)
        {
            if (tparam->ty == Tvector)
            {
                TypeVector *tp = (TypeVector *)tparam;
                result = deduceType(t->basetype, sc, tp->basetype, parameters, dedtypes, wm);
                return;
            }
            visit((Type *)t);
        }

        void visit(TypeDArray *t)
        {
            visit((Type *)t);
        }

        void visit(TypeSArray *t)
        {
            // Extra check that array dimensions must match
            if (tparam)
            {
                if (tparam->ty == Tarray)
                {
                    MATCH m = deduceType(t->next, sc, tparam->nextOf(), parameters, dedtypes, wm);
                    result = (m >= MATCHconst) ? MATCHconvert : MATCHnomatch;
                    return;
                }

                TemplateParameter *tp = NULL;
                Expression *edim = NULL;
                size_t i;
                if (tparam->ty == Tsarray)
                {
                    TypeSArray *tsa = (TypeSArray *)tparam;
                    if (tsa->dim->op == TOKvar &&
                        ((VarExp *)tsa->dim)->var->storage_class & STCtemplateparameter)
                    {
                        Identifier *id = ((VarExp *)tsa->dim)->var->ident;
                        i = templateIdentifierLookup(id, parameters);
                        assert(i != IDX_NOTFOUND);
                        tp = (*parameters)[i];
                    }
                    else
                        edim = tsa->dim;
                }
                else if (tparam->ty == Taarray)
                {
                    TypeAArray *taa = (TypeAArray *)tparam;
                    i = templateParameterLookup(taa->index, parameters);
                    if (i != IDX_NOTFOUND)
                        tp = (*parameters)[i];
                    else
                    {
                        Expression *e;
                        Type *tx;
                        Dsymbol *s;
                        taa->index->resolve(Loc(), sc, &e, &tx, &s);
                        edim = s ? getValue(s) : getValue(e);
                    }
                }
                if ((tp && tp->matchArg(sc, t->dim, i, parameters, dedtypes, NULL)) ||
                    (edim && edim->toInteger() == t->dim->toInteger()))
                {
                    result = deduceType(t->next, sc, tparam->nextOf(), parameters, dedtypes, wm);
                    return;
                }
            }
            visit((Type *)t);
            return;

            result = MATCHnomatch;
        }

        void visit(TypeAArray *t)
        {
            // Extra check that index type must match
            if (tparam && tparam->ty == Taarray)
            {
                TypeAArray *tp = (TypeAArray *)tparam;
                if (!deduceType(t->index, sc, tp->index, parameters, dedtypes))
                {
                    result = MATCHnomatch;
                    return;
                }
            }
            visit((Type *)t);
        }

        void visit(TypeFunction *t)
        {
            //printf("TypeFunction::deduceType()\n");
            //printf("\tthis   = %d, ", t->ty); t->print();
            //printf("\ttparam = %d, ", tparam->ty); tparam->print();

            // Extra check that function characteristics must match
            if (tparam && tparam->ty == Tfunction)
            {
                TypeFunction *tp = (TypeFunction *)tparam;
                if (t->varargs != tp->varargs ||
                    t->linkage != tp->linkage)
                {
                    result = MATCHnomatch;
                    return;
                }

                size_t nfargs = Parameter::dim(t->parameters);
                size_t nfparams = Parameter::dim(tp->parameters);

                // bug 2579 fix: Apply function parameter storage classes to parameter types
                for (size_t i = 0; i < nfparams; i++)
                {
                    Parameter *fparam = Parameter::getNth(tp->parameters, i);
                    fparam->type = fparam->type->addStorageClass(fparam->storageClass);
                    fparam->storageClass &= ~(STC_TYPECTOR | STCin);
                }
                //printf("\t-> this   = %d, ", t->ty); t->print();
                //printf("\t-> tparam = %d, ", tparam->ty); tparam->print();

                /* See if tuple match
                 */
                if (nfparams > 0 && nfargs >= nfparams - 1)
                {
                    /* See if 'A' of the template parameter matches 'A'
                     * of the type of the last function parameter.
                     */
                    Parameter *fparam = Parameter::getNth(tp->parameters, nfparams - 1);
                    assert(fparam);
                    assert(fparam->type);
                    if (fparam->type->ty != Tident)
                        goto L1;
                    TypeIdentifier *tid = (TypeIdentifier *)fparam->type;
                    if (tid->idents.dim)
                        goto L1;

                    /* Look through parameters to find tuple matching tid->ident
                     */
                    size_t tupi = 0;
                    for (; 1; tupi++)
                    {
                        if (tupi == parameters->dim)
                            goto L1;
                        TemplateParameter *tx = (*parameters)[tupi];
                        TemplateTupleParameter *tup = tx->isTemplateTupleParameter();
                        if (tup && tup->ident->equals(tid->ident))
                            break;
                    }

                    /* The types of the function arguments [nfparams - 1 .. nfargs]
                     * now form the tuple argument.
                     */
                    size_t tuple_dim = nfargs - (nfparams - 1);

                    /* See if existing tuple, and whether it matches or not
                     */
                    RootObject *o = (*dedtypes)[tupi];
                    if (o)
                    {
                        // Existing deduced argument must be a tuple, and must match
                        Tuple *tup = isTuple(o);
                        if (!tup || tup->objects.dim != tuple_dim)
                        {
                            result = MATCHnomatch;
                            return;
                        }
                        for (size_t i = 0; i < tuple_dim; i++)
                        {
                            Parameter *arg = Parameter::getNth(t->parameters, nfparams - 1 + i);
                            if (!arg->type->equals(tup->objects[i]))
                            {
                                result = MATCHnomatch;
                                return;
                            }
                        }
                    }
                    else
                    {
                        // Create new tuple
                        Tuple *tup = new Tuple();
                        tup->objects.setDim(tuple_dim);
                        for (size_t i = 0; i < tuple_dim; i++)
                        {
                            Parameter *arg = Parameter::getNth(t->parameters, nfparams - 1 + i);
                            tup->objects[i] = arg->type;
                        }
                        (*dedtypes)[tupi] = tup;
                    }
                    nfparams--; // don't consider the last parameter for type deduction
                    goto L2;
                }

            L1:
                if (nfargs != nfparams)
                {
                    result = MATCHnomatch;
                    return;
                }
            L2:
                for (size_t i = 0; i < nfparams; i++)
                {
                    Parameter *a = Parameter::getNth(t->parameters, i);
                    Parameter *ap = Parameter::getNth(tp->parameters, i);

                    if (!a->isCovariant(t->isref, ap) ||
                        !deduceType(a->type, sc, ap->type, parameters, dedtypes))
                    {
                        result = MATCHnomatch;
                        return;
                    }
                }
            }
            visit((Type *)t);
        }

        void visit(TypeIdentifier *t)
        {
            // Extra check
            if (tparam && tparam->ty == Tident)
            {
                TypeIdentifier *tp = (TypeIdentifier *)tparam;

                for (size_t i = 0; i < t->idents.dim; i++)
                {
                    RootObject *id1 = t->idents[i];
                    RootObject *id2 = tp->idents[i];

                    if (!id1->equals(id2))
                    {
                        result = MATCHnomatch;
                        return;
                    }
                }
            }
            visit((Type *)t);
        }

        void visit(TypeInstance *t)
        {
            // Extra check
            if (tparam && tparam->ty == Tinstance && t->tempinst->tempdecl)
            {
                TemplateDeclaration *tempdecl = t->tempinst->tempdecl->isTemplateDeclaration();
                assert(tempdecl);

                TypeInstance *tp = (TypeInstance *)tparam;

                //printf("tempinst->tempdecl = %p\n", tempdecl);
                //printf("tp->tempinst->tempdecl = %p\n", tp->tempinst->tempdecl);
                if (!tp->tempinst->tempdecl)
                {
                    //printf("tp->tempinst->name = '%s'\n", tp->tempinst->name->toChars());

                    /* Handle case of:
                     *  template Foo(T : sa!(T), alias sa)
                     */
                    size_t i = templateIdentifierLookup(tp->tempinst->name, parameters);
                    if (i == IDX_NOTFOUND)
                    {
                        /* Didn't find it as a parameter identifier. Try looking
                         * it up and seeing if is an alias. See Bugzilla 1454
                         */
                        TypeIdentifier *tid = new TypeIdentifier(tp->loc, tp->tempinst->name);
                        Type *tx;
                        Expression *e;
                        Dsymbol *s;
                        tid->resolve(tp->loc, sc, &e, &tx, &s);
                        if (tx)
                        {
                            s = tx->toDsymbol(sc);
                            if (TemplateInstance *ti = s ? s->parent->isTemplateInstance() : NULL)
                            {
                                // Bugzilla 14290: Try to match with ti->tempecl,
                                // only when ti is an enclosing instance.
                                Dsymbol *p = sc->parent;
                                while (p && p != ti)
                                    p = p->parent;
                                if (p)
                                    s = ti->tempdecl;
                            }
                        }
                        if (s)
                        {
                            s = s->toAlias();
                            TemplateDeclaration *td = s->isTemplateDeclaration();
                            if (td)
                            {
                                if (td->overroot)
                                    td = td->overroot;
                                for (; td; td = td->overnext)
                                {
                                    if (td == tempdecl)
                                        goto L2;
                                }
                            }
                        }
                        goto Lnomatch;
                    }
                    TemplateParameter *tpx = (*parameters)[i];
                    if (!tpx->matchArg(sc, tempdecl, i, parameters, dedtypes, NULL))
                        goto Lnomatch;
                }
                else if (tempdecl != tp->tempinst->tempdecl)
                    goto Lnomatch;

              L2:

                for (size_t i = 0; 1; i++)
                {
                    //printf("\ttest: tempinst->tiargs[%d]\n", i);
                    RootObject *o1 = NULL;
                    if (i < t->tempinst->tiargs->dim)
                        o1 = (*t->tempinst->tiargs)[i];
                    else if (i < t->tempinst->tdtypes.dim && i < tp->tempinst->tiargs->dim)
                    {
                        // Pick up default arg
                        o1 = t->tempinst->tdtypes[i];
                    }
                    else if (i >= tp->tempinst->tiargs->dim)
                        break;

                    if (i >= tp->tempinst->tiargs->dim)
                    {
                        size_t dim = tempdecl->parameters->dim - (tempdecl->isVariadic() ? 1 : 0);
                        while (i < dim && ((*tempdecl->parameters)[i]->dependent ||
                                           (*tempdecl->parameters)[i]->hasDefaultArg()))
                        {
                            i++;
                        }
                        if (i >= dim)
                            break;  // match if all remained parameters are dependent
                        goto Lnomatch;
                    }

                    RootObject *o2 = (*tp->tempinst->tiargs)[i];
                    Type *t2 = isType(o2);

                    size_t j = (t2 && t2->ty == Tident && i == tp->tempinst->tiargs->dim - 1)
                        ? templateParameterLookup(t2, parameters) : IDX_NOTFOUND;
                    if (j != IDX_NOTFOUND && j == parameters->dim - 1 &&
                        (*parameters)[j]->isTemplateTupleParameter())
                    {
                        /* Given:
                         *  struct A(B...) {}
                         *  alias A!(int, float) X;
                         *  static if (is(X Y == A!(Z), Z...)) {}
                         * deduce that Z is a tuple(int, float)
                         */

                        /* Create tuple from remaining args
                         */
                        Tuple *vt = new Tuple();
                        size_t vtdim = (tempdecl->isVariadic()
                                        ? t->tempinst->tiargs->dim : t->tempinst->tdtypes.dim) - i;
                        vt->objects.setDim(vtdim);
                        for (size_t k = 0; k < vtdim; k++)
                        {
                            RootObject *o;
                            if (k < t->tempinst->tiargs->dim)
                                o = (*t->tempinst->tiargs)[i + k];
                            else    // Pick up default arg
                                o = t->tempinst->tdtypes[i + k];
                            vt->objects[k] = o;
                        }

                        Tuple *v = (Tuple *)(*dedtypes)[j];
                        if (v)
                        {
                            if (!match(v, vt))
                                goto Lnomatch;
                        }
                        else
                            (*dedtypes)[j] = vt;
                        break;
                    }
                    else if (!o1)
                        break;

                    Type *t1 = isType(o1);
                    Dsymbol *s1 = isDsymbol(o1);
                    Dsymbol *s2 = isDsymbol(o2);
                    Expression *e1 = s1 ? getValue(s1) : getValue(isExpression(o1));
                    Expression *e2 = isExpression(o2);

                    if (t1 && t2)
                    {
                        if (!deduceType(t1, sc, t2, parameters, dedtypes))
                            goto Lnomatch;
                    }
                    else if (e1 && e2)
                    {
                    Le:
                        e1 = e1->ctfeInterpret();

                        /* If it is one of the template parameters for this template,
                         * we should not attempt to interpret it. It already has a value.
                         */
                        if (e2->op == TOKvar &&
                            (((VarExp *)e2)->var->storage_class & STCtemplateparameter))
                        {
                            /*
                             * (T:Number!(e2), int e2)
                             */
                            j = templateIdentifierLookup(((VarExp *)e2)->var->ident, parameters);
                            if (j != IDX_NOTFOUND)
                                goto L1;
                            // The template parameter was not from this template
                            // (it may be from a parent template, for example)
                        }

                        e2 = ::semantic(e2, sc);      // Bugzilla 13417
                        e2 = e2->ctfeInterpret();

                        //printf("e1 = %s, type = %s %d\n", e1->toChars(), e1->type->toChars(), e1->type->ty);
                        //printf("e2 = %s, type = %s %d\n", e2->toChars(), e2->type->toChars(), e2->type->ty);
                        if (!e1->equals(e2))
                        {
                            if (!e2->implicitConvTo(e1->type))
                                goto Lnomatch;

                            e2 = e2->implicitCastTo(sc, e1->type);
                            e2 = e2->ctfeInterpret();
                            if (!e1->equals(e2))
                                goto Lnomatch;
                        }
                    }
                    else if (e1 && t2 && t2->ty == Tident)
                    {
                        j = templateParameterLookup(t2, parameters);
                    L1:
                        if (j == IDX_NOTFOUND)
                        {
                            t2->resolve(((TypeIdentifier *)t2)->loc, sc, &e2, &t2, &s2);
                            if (e2)
                                goto Le;
                            goto Lnomatch;
                        }
                        if (!(*parameters)[j]->matchArg(sc, e1, j, parameters, dedtypes, NULL))
                            goto Lnomatch;
                    }
                    else if (s1 && s2)
                    {
                    Ls:
                        if (!s1->equals(s2))
                            goto Lnomatch;
                    }
                    else if (s1 && t2 && t2->ty == Tident)
                    {
                        j = templateParameterLookup(t2, parameters);
                        if (j == IDX_NOTFOUND)
                        {
                            t2->resolve(((TypeIdentifier *)t2)->loc, sc, &e2, &t2, &s2);
                            if (s2)
                                goto Ls;
                            goto Lnomatch;
                        }
                        if (!(*parameters)[j]->matchArg(sc, s1, j, parameters, dedtypes, NULL))
                            goto Lnomatch;
                    }
                    else
                        goto Lnomatch;
                }
            }
            visit((Type *)t);
            return;

        Lnomatch:
            //printf("no match\n");
            result = MATCHnomatch;
        }

        void visit(TypeStruct *t)
        {
            /* If this struct is a template struct, and we're matching
             * it against a template instance, convert the struct type
             * to a template instance, too, and try again.
             */
            TemplateInstance *ti = t->sym->parent->isTemplateInstance();

            if (tparam && tparam->ty == Tinstance)
            {
                if (ti && ti->toAlias() == t->sym)
                {
                    TypeInstance *tx = new TypeInstance(Loc(), ti);
                    result = deduceType(tx, sc, tparam, parameters, dedtypes, wm);
                    return;
                }

                /* Match things like:
                 *  S!(T).foo
                 */
                TypeInstance *tpi = (TypeInstance *)tparam;
                if (tpi->idents.dim)
                {
                    RootObject *id = tpi->idents[tpi->idents.dim - 1];
                    if (id->dyncast() == DYNCAST_IDENTIFIER && t->sym->ident->equals((Identifier *)id))
                    {
                        Type *tparent = t->sym->parent->getType();
                        if (tparent)
                        {
                            /* Slice off the .foo in S!(T).foo
                             */
                            tpi->idents.dim--;
                            result = deduceType(tparent, sc, tpi, parameters, dedtypes, wm);
                            tpi->idents.dim++;
                            return;
                        }
                    }
                }
            }

            // Extra check
            if (tparam && tparam->ty == Tstruct)
            {
                TypeStruct *tp = (TypeStruct *)tparam;

                //printf("\t%d\n", (MATCH) t->implicitConvTo(tp));
                if (wm && t->deduceWild(tparam, false))
                {
                    result = MATCHconst;
                    return;
                }
                result = t->implicitConvTo(tp);
                return;
            }
            visit((Type *)t);
        }

        void visit(TypeEnum *t)
        {
            // Extra check
            if (tparam && tparam->ty == Tenum)
            {
                TypeEnum *tp = (TypeEnum *)tparam;
                if (t->sym == tp->sym)
                    visit((Type *)t);
                else
                    result = MATCHnomatch;
                return;
            }
            Type *tb = t->toBasetype();
            if (tb->ty == tparam->ty ||
                (tb->ty == Tsarray && tparam->ty == Taarray))
            {
                result = deduceType(tb, sc, tparam, parameters, dedtypes, wm);
                return;
            }
            visit((Type *)t);
        }

        /* Helper for TypeClass::deduceType().
         * Classes can match with implicit conversion to a base class or interface.
         * This is complicated, because there may be more than one base class which
         * matches. In such cases, one or more parameters remain ambiguous.
         * For example,
         *
         *   interface I(X, Y) {}
         *   class C : I(uint, double), I(char, double) {}
         *   C x;
         *   foo(T, U)( I!(T, U) x)
         *
         *   deduces that U is double, but T remains ambiguous (could be char or uint).
         *
         * Given a baseclass b, and initial deduced types 'dedtypes', this function
         * tries to match tparam with b, and also tries all base interfaces of b.
         * If a match occurs, numBaseClassMatches is incremented, and the new deduced
         * types are ANDed with the current 'best' estimate for dedtypes.
         */
        static void deduceBaseClassParameters(BaseClass *b,
            Scope *sc, Type *tparam, TemplateParameters *parameters, Objects *dedtypes,
            Objects *best, int &numBaseClassMatches)
        {
            TemplateInstance *parti = b->sym ? b->sym->parent->isTemplateInstance() : NULL;
            if (parti)
            {
                // Make a temporary copy of dedtypes so we don't destroy it
                Objects *tmpdedtypes = new Objects();
                tmpdedtypes->setDim(dedtypes->dim);
                memcpy(tmpdedtypes->tdata(), dedtypes->tdata(), dedtypes->dim * sizeof(void *));

                TypeInstance *t = new TypeInstance(Loc(), parti);
                MATCH m = deduceType(t, sc, tparam, parameters, tmpdedtypes);
                if (m > MATCHnomatch)
                {
                    // If this is the first ever match, it becomes our best estimate
                    if (numBaseClassMatches==0)
                        memcpy(best->tdata(), tmpdedtypes->tdata(), tmpdedtypes->dim * sizeof(void *));
                    else for (size_t k = 0; k < tmpdedtypes->dim; ++k)
                    {
                        // If we've found more than one possible type for a parameter,
                        // mark it as unknown.
                        if ((*tmpdedtypes)[k] != (*best)[k])
                            (*best)[k] = (*dedtypes)[k];
                    }
                    ++numBaseClassMatches;
                }
            }
            // Now recursively test the inherited interfaces
            for (size_t j = 0; j < b->baseInterfaces.length; ++j)
            {
                BaseClass *bi = &b->baseInterfaces.ptr[j];
                deduceBaseClassParameters(bi,
                    sc, tparam, parameters, dedtypes,
                    best, numBaseClassMatches);
            }

        }

        void visit(TypeClass *t)
        {
            //printf("TypeClass::deduceType(this = %s)\n", t->toChars());

            /* If this class is a template class, and we're matching
             * it against a template instance, convert the class type
             * to a template instance, too, and try again.
             */
            TemplateInstance *ti = t->sym->parent->isTemplateInstance();

            if (tparam && tparam->ty == Tinstance)
            {
                if (ti && ti->toAlias() == t->sym)
                {
                    TypeInstance *tx = new TypeInstance(Loc(), ti);
                    MATCH m = deduceType(tx, sc, tparam, parameters, dedtypes, wm);
                    // Even if the match fails, there is still a chance it could match
                    // a base class.
                    if (m != MATCHnomatch)
                    {
                        result = m;
                        return;
                    }
                }

                /* Match things like:
                 *  S!(T).foo
                 */
                TypeInstance *tpi = (TypeInstance *)tparam;
                if (tpi->idents.dim)
                {
                    RootObject *id = tpi->idents[tpi->idents.dim - 1];
                    if (id->dyncast() == DYNCAST_IDENTIFIER && t->sym->ident->equals((Identifier *)id))
                    {
                        Type *tparent = t->sym->parent->getType();
                        if (tparent)
                        {
                            /* Slice off the .foo in S!(T).foo
                             */
                            tpi->idents.dim--;
                            result = deduceType(tparent, sc, tpi, parameters, dedtypes, wm);
                            tpi->idents.dim++;
                            return;
                        }
                    }
                }

                // If it matches exactly or via implicit conversion, we're done
                visit((Type *)t);
                if (result != MATCHnomatch)
                    return;

                /* There is still a chance to match via implicit conversion to
                 * a base class or interface. Because there could be more than one such
                 * match, we need to check them all.
                 */

                int numBaseClassMatches = 0; // Have we found an interface match?

                // Our best guess at dedtypes
                Objects *best = new Objects();
                best->setDim(dedtypes->dim);

                ClassDeclaration *s = t->sym;
                while (s && s->baseclasses->dim > 0)
                {
                    // Test the base class
                    deduceBaseClassParameters((*s->baseclasses)[0],
                        sc, tparam, parameters, dedtypes,
                        best, numBaseClassMatches);

                    // Test the interfaces inherited by the base class
                    for (size_t i = 0; i < s->interfaces.length; ++i)
                    {
                        BaseClass *b = s->interfaces.ptr[i];
                        deduceBaseClassParameters(b, sc, tparam, parameters, dedtypes,
                            best, numBaseClassMatches);
                    }
                    s = (*s->baseclasses)[0]->sym;
                }

                if (numBaseClassMatches == 0)
                {
                    result = MATCHnomatch;
                    return;
                }

                // If we got at least one match, copy the known types into dedtypes
                memcpy(dedtypes->tdata(), best->tdata(), best->dim * sizeof(void *));
                result = MATCHconvert;
                return;
            }

            // Extra check
            if (tparam && tparam->ty == Tclass)
            {
                TypeClass *tp = (TypeClass *)tparam;

                //printf("\t%d\n", (MATCH) t->implicitConvTo(tp));
                if (wm && t->deduceWild(tparam, false))
                {
                    result = MATCHconst;
                    return;
                }
                result = t->implicitConvTo(tp);
                return;
            }
            visit((Type *)t);
        }

        void visit(Expression *e)
        {
            //printf("Expression::deduceType(e = %s)\n", e->toChars());
            size_t i = templateParameterLookup(tparam, parameters);
            if (i == IDX_NOTFOUND || ((TypeIdentifier *)tparam)->idents.dim > 0)
            {
                if (e == emptyArrayElement && tparam->ty == Tarray)
                {
                    Type *tn = ((TypeNext *)tparam)->next;
                    result = deduceType(emptyArrayElement, sc, tn, parameters, dedtypes, wm);
                    return;
                }
                e->type->accept(this);
                return;
            }

            TemplateTypeParameter *tp = (*parameters)[i]->isTemplateTypeParameter();
            if (!tp)
                return; // nomatch

            if (e == emptyArrayElement)
            {
                if ((*dedtypes)[i])
                {
                    result = MATCHexact;
                    return;
                }
                if (tp->defaultType)
                {
                    tp->defaultType->accept(this);
                    return;
                }
            }

            Type *at = (Type *)(*dedtypes)[i];
            Type *tt;
            if (unsigned char wx = deduceWildHelper(e->type, &tt, tparam))
            {
                *wm |= wx;
                result = MATCHconst;
            }
            else if (MATCH m = deduceTypeHelper(e->type, &tt, tparam))
            {
                result = m;
            }
            else
                return; // nomatch

            // expression vs (none)
            if (!at)
            {
                (*dedtypes)[i] = new TypeDeduced(tt, e, tparam);
                return;
            }

            TypeDeduced *xt = NULL;
            if (at->ty == Tnone)
            {
                xt = (TypeDeduced *)at;
                at = xt->tded;
            }

            // From previous matched expressions to current deduced type
            MATCH match1 = xt ? xt->matchAll(tt) : MATCHnomatch;

            // From current expresssion to previous deduced type
            Type *pt = at->addMod(tparam->mod);
            if (*wm)
                pt = pt->substWildTo(*wm);
            MATCH match2 = e->implicitConvTo(pt);

            if (match1 > MATCHnomatch && match2 > MATCHnomatch)
            {
                if (at->implicitConvTo(tt) <= MATCHnomatch)
                    match1 = MATCHnomatch;  // Prefer at
                else if (tt->implicitConvTo(at) <= MATCHnomatch)
                    match2 = MATCHnomatch;  // Prefer tt
                else if (tt->isTypeBasic() && tt->ty == at->ty && tt->mod != at->mod)
                {
                    if (!tt->isMutable() && !at->isMutable())
                        tt = tt->mutableOf()->addMod(MODmerge(tt->mod, at->mod));
                    else if (tt->isMutable())
                    {
                        if (at->mod == 0)   // Prefer unshared
                            match1 = MATCHnomatch;
                        else
                            match2 = MATCHnomatch;
                    }
                    else if (at->isMutable())
                    {
                        if (tt->mod == 0)   // Prefer unshared
                            match2 = MATCHnomatch;
                        else
                            match1 = MATCHnomatch;
                    }
                    //printf("tt = %s, at = %s\n", tt->toChars(), at->toChars());
                }
                else
                {
                    match1 = MATCHnomatch;
                    match2 = MATCHnomatch;
                }
            }
            if (match1 > MATCHnomatch)
            {
                // Prefer current match: tt
                if (xt)
                    xt->update(tt, e, tparam);
                else
                    (*dedtypes)[i] = tt;
                result = match1;
                return;
            }
            if (match2 > MATCHnomatch)
            {
                // Prefer previous match: (*dedtypes)[i]
                if (xt)
                    xt->update(e, tparam);
                result = match2;
                return;
            }

            /* Deduce common type
             */
            if (Type *t = rawTypeMerge(at, tt))
            {
                if (xt)
                    xt->update(t, e, tparam);
                else
                    (*dedtypes)[i] = t;

                pt = tt->addMod(tparam->mod);
                if (*wm)
                    pt = pt->substWildTo(*wm);
                result = e->implicitConvTo(pt);
                return;
            }

            result = MATCHnomatch;
        }

        MATCH deduceEmptyArrayElement()
        {
            if (!emptyArrayElement)
            {
                emptyArrayElement = new IdentifierExp(Loc(), Id::p);    // dummy
                emptyArrayElement->type = Type::tvoid;
            }
            assert(tparam->ty == Tarray);

            Type *tn = ((TypeNext *)tparam)->next;
            return deduceType(emptyArrayElement, sc, tn, parameters, dedtypes, wm);
        }

        void visit(NullExp *e)
        {
            if (tparam->ty == Tarray && e->type->ty == Tnull)
            {
                // tparam:T[] <- e:null (void[])
                result = deduceEmptyArrayElement();
                return;
            }
            visit((Expression *)e);
        }

        void visit(StringExp *e)
        {
            Type *taai;
            if (e->type->ty == Tarray &&
                (tparam->ty == Tsarray ||
                 (tparam->ty == Taarray && (taai = ((TypeAArray *)tparam)->index)->ty == Tident &&
                  ((TypeIdentifier *)taai)->idents.dim == 0)))
            {
                // Consider compile-time known boundaries
                e->type->nextOf()->sarrayOf(e->len)->accept(this);
                return;
            }
            visit((Expression *)e);
        }

        void visit(ArrayLiteralExp *e)
        {
            if ((!e->elements || !e->elements->dim) &&
                e->type->toBasetype()->nextOf()->ty == Tvoid &&
                tparam->ty == Tarray)
            {
                // tparam:T[] <- e:[] (void[])
                result = deduceEmptyArrayElement();
                return;
            }

            if (tparam->ty == Tarray && e->elements && e->elements->dim)
            {
                Type *tn = ((TypeDArray *)tparam)->next;
                result = MATCHexact;
                if (e->basis)
                {
                    MATCH m = deduceType(e->basis, sc, tn, parameters, dedtypes, wm);
                    if (m < result)
                        result = m;
                }
                for (size_t i = 0; i < e->elements->dim; i++)
                {
                    if (result <= MATCHnomatch)
                        break;
                    Expression *el = (*e->elements)[i];
                    if (!el)
                        continue;
                    MATCH m = deduceType(el, sc, tn, parameters, dedtypes, wm);
                    if (m < result)
                        result = m;
                }
                return;
            }

            Type *taai;
            if (e->type->ty == Tarray &&
                (tparam->ty == Tsarray ||
                 (tparam->ty == Taarray && (taai = ((TypeAArray *)tparam)->index)->ty == Tident &&
                  ((TypeIdentifier *)taai)->idents.dim == 0)))
            {
                // Consider compile-time known boundaries
                e->type->nextOf()->sarrayOf(e->elements->dim)->accept(this);
                return;
            }
            visit((Expression *)e);
        }

        void visit(AssocArrayLiteralExp *e)
        {
            if (tparam->ty == Taarray && e->keys && e->keys->dim)
            {
                TypeAArray *taa = (TypeAArray *)tparam;
                result = MATCHexact;
                for (size_t i = 0; i < e->keys->dim; i++)
                {
                    MATCH m1 = deduceType((*e->keys)[i], sc, taa->index, parameters, dedtypes, wm);
                    if (m1 < result)
                        result = m1;
                    if (result <= MATCHnomatch)
                        break;
                    MATCH m2 = deduceType((*e->values)[i], sc, taa->next, parameters, dedtypes, wm);
                    if (m2 < result)
                        result = m2;
                    if (result <= MATCHnomatch)
                        break;
                }
                return;
            }
            visit((Expression *)e);
        }

        void visit(FuncExp *e)
        {
            //printf("e->type = %s, tparam = %s\n", e->type->toChars(), tparam->toChars());
            if (e->td)
            {
                Type *to = tparam;
                if (!to->nextOf() || to->nextOf()->ty != Tfunction)
                    return;
                TypeFunction *tof = (TypeFunction *)to->nextOf();

                // Parameter types inference from 'tof'
                assert(e->td->_scope);
                TypeFunction *tf = (TypeFunction *)e->fd->type;
                //printf("\ttof = %s\n", tof->toChars());
                //printf("\ttf  = %s\n", tf->toChars());
                size_t dim = Parameter::dim(tf->parameters);

                if (Parameter::dim(tof->parameters) != dim ||
                    tof->varargs != tf->varargs)
                    return;

                Objects *tiargs = new Objects();
                tiargs->reserve(e->td->parameters->dim);

                for (size_t i = 0; i < e->td->parameters->dim; i++)
                {
                    TemplateParameter *tp = (*e->td->parameters)[i];
                    size_t u = 0;
                    for (; u < dim; u++)
                    {
                        Parameter *p = Parameter::getNth(tf->parameters, u);
                        if (p->type->ty == Tident &&
                            ((TypeIdentifier *)p->type)->ident == tp->ident)
                        {
                            break;
                        }
                    }
                    assert(u < dim);
                    Parameter *pto = Parameter::getNth(tof->parameters, u);
                    if (!pto)
                        break;
                    Type *t = pto->type->syntaxCopy();  // Bugzilla 11774
                    if (reliesOnTident(t, parameters, inferStart))
                        return;
                    t = t->semantic(e->loc, sc);
                    if (t->ty == Terror)
                        return;
                    tiargs->push(t);
                }

                // Set target of return type inference
                if (!tf->next && tof->next)
                    e->fd->treq = tparam;

                TemplateInstance *ti = new TemplateInstance(e->loc, e->td, tiargs);
                Expression *ex = new ScopeExp(e->loc, ti);
                ex = ::semantic(ex, e->td->_scope);

                // Reset inference target for the later re-semantic
                e->fd->treq = NULL;

                if (ex->op == TOKerror)
                    return;
                if (ex->op != TOKfunction)
                    return;
                visit(ex->type);
                return;
            }

            Type *t = e->type;

            if (t->ty == Tdelegate && tparam->ty == Tpointer)
                return;

            // Allow conversion from implicit function pointer to delegate
            if (e->tok == TOKreserved &&
                t->ty == Tpointer && tparam->ty == Tdelegate)
            {
                TypeFunction *tf = (TypeFunction *)t->nextOf();
                t = (new TypeDelegate(tf))->merge();
            }
            //printf("tparam = %s <= e->type = %s, t = %s\n", tparam->toChars(), e->type->toChars(), t->toChars());
            visit(t);
        }

        void visit(SliceExp *e)
        {
            Type *taai;
            if (e->type->ty == Tarray &&
                (tparam->ty == Tsarray ||
                 (tparam->ty == Taarray && (taai = ((TypeAArray *)tparam)->index)->ty == Tident &&
                  ((TypeIdentifier *)taai)->idents.dim == 0)))
            {
                // Consider compile-time known boundaries
                if (Type *tsa = toStaticArrayType(e))
                {
                    tsa->accept(this);
                    return;
                }
            }
            visit((Expression *)e);
        }

        void visit(CommaExp *e)
        {
            ((CommaExp *)e)->e2->accept(this);
        }
    };

    DeduceType v(sc, tparam, parameters, dedtypes, wm, inferStart);
    if (Type *t = isType(o))
        t->accept(&v);
    else
    {
        assert(isExpression(o) && wm);
        ((Expression *)o)->accept(&v);
    }
    return v.result;
}

/*******************************
 * Input:
 *      t           Tested type, if NULL, returns NULL.
 *      tparams     Optional template parameters.
 *                  == NULL:
 *                      If one of the subtypes of this type is a TypeIdentifier,
 *                      i.e. it's an unresolved type, return that type.
 *                  != NULL:
 *                      Only when the TypeIdentifier is one of template parameters,
 *                      return that type.
 */

bool reliesOnTident(Type *t, TemplateParameters *tparams, size_t iStart)
{
    class ReliesOnTident : public Visitor
    {
    public:
        TemplateParameters *tparams;
        size_t iStart;
        bool result;

        ReliesOnTident(TemplateParameters *tparams, size_t iStart)
            : tparams(tparams), iStart(iStart)
        {
            result = false;
        }

        void visit(Type *)
        {
        }

        void visit(TypeNext *t)
        {
            t->next->accept(this);
        }

        void visit(TypeVector *t)
        {
            t->basetype->accept(this);
        }

        void visit(TypeAArray *t)
        {
            visit((TypeNext *)t);
            if (!result)
                t->index->accept(this);
        }

        void visit(TypeFunction *t)
        {
            size_t dim = Parameter::dim(t->parameters);
            for (size_t i = 0; i < dim; i++)
            {
                Parameter *fparam = Parameter::getNth(t->parameters, i);
                fparam->type->accept(this);
                if (result)
                    return;
            }
            if (t->next)
                t->next->accept(this);
        }

        void visit(TypeIdentifier *t)
        {
            if (!tparams)
            {
                result = true;
                return;
            }

            for (size_t i = iStart; i < tparams->dim; i++)
            {
                TemplateParameter *tp = (*tparams)[i];
                if (tp->ident->equals(t->ident))
                {
                    result = true;
                    return;
                }
            }
        }

        void visit(TypeInstance *t)
        {
            if (!tparams)
                return;

            for (size_t i = iStart; i < tparams->dim; i++)
            {
                TemplateParameter *tp = (*tparams)[i];
                if (t->tempinst->name == tp->ident)
                {
                    result = true;
                    return;
                }
            }
            if (!t->tempinst->tiargs)
                return;
            for (size_t i = 0; i < t->tempinst->tiargs->dim; i++)
            {
                Type *ta = isType((*t->tempinst->tiargs)[i]);
                if (ta)
                {
                    ta->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(TypeTypeof *t)
        {
            //printf("TypeTypeof::reliesOnTident('%s')\n", t->toChars());
            t->exp->accept(this);
        }

        void visit(TypeTuple *t)
        {
            if (t->arguments)
            {
                for (size_t i = 0; i < t->arguments->dim; i++)
                {
                    Parameter *arg = (*t->arguments)[i];
                    arg->type->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(Expression *)
        {
            //printf("Expression::reliesOnTident('%s')\n", e->toChars());
        }

        void visit(IdentifierExp *e)
        {
            //printf("IdentifierExp::reliesOnTident('%s')\n", e->toChars());
            for (size_t i = iStart; i < tparams->dim; i++)
            {
                TemplateParameter *tp = (*tparams)[i];
                if (e->ident == tp->ident)
                {
                    result = true;
                    return;
                }
            }
        }

        void visit(TupleExp *e)
        {
            //printf("TupleExp::reliesOnTident('%s')\n", e->toChars());
            if (e->exps)
            {
                for (size_t i = 0; i < e->exps->dim; i++)
                {
                    Expression *ea = (*e->exps)[i];
                    ea->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(ArrayLiteralExp *e)
        {
            //printf("ArrayLiteralExp::reliesOnTident('%s')\n", e->toChars());
            if (e->elements)
            {
                for (size_t i = 0; i < e->elements->dim; i++)
                {
                    Expression *el = (*e->elements)[i];
                    el->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(AssocArrayLiteralExp *e)
        {
            //printf("AssocArrayLiteralExp::reliesOnTident('%s')\n", e->toChars());
            for (size_t i = 0; i < e->keys->dim; i++)
            {
                Expression *ek = (*e->keys)[i];
                ek->accept(this);
                if (result)
                    return;
            }
            for (size_t i = 0; i < e->values->dim; i++)
            {
                Expression *ev = (*e->values)[i];
                ev->accept(this);
                if (result)
                    return;
            }
        }

        void visit(StructLiteralExp *e)
        {
            //printf("StructLiteralExp::reliesOnTident('%s')\n", e->toChars());
            if (e->elements)
            {
                for (size_t i = 0; i < e->elements->dim; i++)
                {
                    Expression *ea = (*e->elements)[i];
                    ea->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(TypeExp *e)
        {
            //printf("TypeExp::reliesOnTident('%s')\n", e->toChars());
            e->type->accept(this);
        }

        void visit(NewExp *e)
        {
            //printf("NewExp::reliesOnTident('%s')\n", e->toChars());
            if (e->thisexp)
                e->thisexp->accept(this);
            if (!result && e->newargs)
            {
                for (size_t i = 0; i < e->newargs->dim; i++)
                {
                    Expression *ea = (*e->newargs)[i];
                    ea->accept(this);
                    if (result)
                        return;
                }
            }
            e->newtype->accept(this);
            if (!result && e->arguments)
            {
                for (size_t i = 0; i < e->arguments->dim; i++)
                {
                    Expression *ea = (*e->arguments)[i];
                    ea->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(NewAnonClassExp *)
        {
            //printf("NewAnonClassExp::reliesOnTident('%s')\n", e->toChars());
            result = true;
        }

        void visit(FuncExp *)
        {
            //printf("FuncExp::reliesOnTident('%s')\n", e->toChars());
            result = true;
        }

        void visit(TypeidExp *e)
        {
            //printf("TypeidExp::reliesOnTident('%s')\n", e->toChars());
            if (Expression *ea = isExpression(e->obj))
                ea->accept(this);
            else if (Type *ta = isType(e->obj))
                ta->accept(this);
        }

        void visit(TraitsExp *e)
        {
            //printf("TraitsExp::reliesOnTident('%s')\n", e->toChars());
            if (e->args)
            {
                for (size_t i = 0; i < e->args->dim; i++)
                {
                    RootObject *oa = (*e->args)[i];
                    if (Expression *ea = isExpression(oa))
                        ea->accept(this);
                    else if (Type *ta = isType(oa))
                        ta->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(IsExp *e)
        {
            //printf("IsExp::reliesOnTident('%s')\n", e->toChars());
            e->targ->accept(this);
        }

        void visit(UnaExp *e)
        {
            //printf("UnaExp::reliesOnTident('%s')\n", e->toChars());
            e->e1->accept(this);
        }

        void visit(DotTemplateInstanceExp *e)
        {
            //printf("DotTemplateInstanceExp::reliesOnTident('%s')\n", e->toChars());
            visit((UnaExp *)e);
            if (!result && e->ti->tiargs)
            {
                for (size_t i = 0; i < e->ti->tiargs->dim; i++)
                {
                    RootObject *oa = (*e->ti->tiargs)[i];
                    if (Expression *ea = isExpression(oa))
                        ea->accept(this);
                    else if (Type *ta = isType(oa))
                        ta->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(CallExp *e)
        {
            //printf("CallExp::reliesOnTident('%s')\n", e->toChars());
            visit((UnaExp *)e);
            if (!result && e->arguments)
            {
                for (size_t i = 0; i < e->arguments->dim; i++)
                {
                    Expression *ea = (*e->arguments)[i];
                    ea->accept(this);
                    if (result)
                        return;
                }
            }
        }

        void visit(CastExp *e)
        {
            //printf("CastExp::reliesOnTident('%s')\n", e->toChars());
            visit((UnaExp *)e);
            // e.to can be null for cast() with no type
            if (!result && e->to)
                e->to->accept(this);
        }

        void visit(SliceExp *e)
        {
            //printf("SliceExp::reliesOnTident('%s')\n", e->toChars());
            visit((UnaExp *)e);
            if (!result && e->lwr)
                e->lwr->accept(this);
            if (!result && e->upr)
                e->upr->accept(this);
        }

        void visit(IntervalExp *e)
        {
            //printf("IntervalExp::reliesOnTident('%s')\n", e->toChars());
            e->lwr->accept(this);
            if (!result)
                e->upr->accept(this);
        }

        void visit(ArrayExp *e)
        {
            //printf("ArrayExp::reliesOnTident('%s')\n", e->toChars());
            visit((UnaExp *)e);
            if (!result && e->arguments)
            {
                for (size_t i = 0; i < e->arguments->dim; i++)
                {
                    Expression *ea = (*e->arguments)[i];
                    ea->accept(this);
                }
            }
        }

        void visit(BinExp *e)
        {
            //printf("BinExp::reliesOnTident('%s')\n", e->toChars());
            e->e1->accept(this);
            if (!result)
                e->e2->accept(this);
        }

        void visit(CondExp *e)
        {
            //printf("BinExp::reliesOnTident('%s')\n", e->toChars());
            e->econd->accept(this);
            if (!result)
                visit((BinExp *)e);
        }
    };

    if (!t)
        return false;

    ReliesOnTident v(tparams, iStart);
    t->accept(&v);
    return v.result;
}

/* ======================== TemplateParameter =============================== */

TemplateParameter::TemplateParameter(Loc loc, Identifier *ident)
{
    this->loc = loc;
    this->ident = ident;
    this->dependent = false;
}

TemplateTypeParameter  *TemplateParameter::isTemplateTypeParameter()
{
    return NULL;
}

TemplateValueParameter *TemplateParameter::isTemplateValueParameter()
{
    return NULL;
}

TemplateAliasParameter *TemplateParameter::isTemplateAliasParameter()
{
    return NULL;
}

TemplateTupleParameter *TemplateParameter::isTemplateTupleParameter()
{
    return NULL;
}

TemplateThisParameter  *TemplateParameter::isTemplateThisParameter()
{
    return NULL;
}

/*******************************************
 * Match to a particular TemplateParameter.
 * Input:
 *      instLoc         location that the template is instantiated.
 *      tiargs[]        actual arguments to template instance
 *      i               i'th argument
 *      parameters[]    template parameters
 *      dedtypes[]      deduced arguments to template instance
 *      *psparam        set to symbol declared and initialized to dedtypes[i]
 */
MATCH TemplateParameter::matchArg(Loc instLoc, Scope *sc, Objects *tiargs,
        size_t i, TemplateParameters *parameters, Objects *dedtypes,
        Declaration **psparam)
{
    RootObject *oarg;

    if (i < tiargs->dim)
        oarg = (*tiargs)[i];
    else
    {
        // Get default argument instead
        oarg = defaultArg(instLoc, sc);
        if (!oarg)
        {
            assert(i < dedtypes->dim);
            // It might have already been deduced
            oarg = (*dedtypes)[i];
            if (!oarg)
                goto Lnomatch;
        }
    }
    return matchArg(sc, oarg, i, parameters, dedtypes, psparam);

Lnomatch:
    if (psparam)
        *psparam = NULL;
    return MATCHnomatch;
}

/* ======================== TemplateTypeParameter =========================== */

// type-parameter

Type *TemplateTypeParameter::tdummy = NULL;

TemplateTypeParameter::TemplateTypeParameter(Loc loc, Identifier *ident, Type *specType,
        Type *defaultType)
    : TemplateParameter(loc, ident)
{
    this->ident = ident;
    this->specType = specType;
    this->defaultType = defaultType;
}

TemplateTypeParameter  *TemplateTypeParameter::isTemplateTypeParameter()
{
    return this;
}

TemplateParameter *TemplateTypeParameter::syntaxCopy()
{
    return new TemplateTypeParameter(loc, ident,
        specType    ? specType->syntaxCopy()    : NULL,
        defaultType ? defaultType->syntaxCopy() : NULL);
}

bool TemplateTypeParameter::declareParameter(Scope *sc)
{
    //printf("TemplateTypeParameter::declareParameter('%s')\n", ident->toChars());
    TypeIdentifier *ti = new TypeIdentifier(loc, ident);
    Declaration *ad = new AliasDeclaration(loc, ident, ti);
    return sc->insert(ad) != NULL;
}

bool TemplateTypeParameter::semantic(Scope *sc, TemplateParameters *parameters)
{
    //printf("TemplateTypeParameter::semantic('%s')\n", ident->toChars());
    if (specType && !reliesOnTident(specType, parameters))
    {
        specType = specType->semantic(loc, sc);
    }
    return !(specType && isError(specType));
}

MATCH TemplateTypeParameter::matchArg(Scope *sc, RootObject *oarg,
        size_t i, TemplateParameters *parameters, Objects *dedtypes,
        Declaration **psparam)
{
    //printf("TemplateTypeParameter::matchArg('%s')\n", ident->toChars());
    MATCH m = MATCHexact;
    Type *ta = isType(oarg);
    if (!ta)
    {
        //printf("%s %p %p %p\n", oarg->toChars(), isExpression(oarg), isDsymbol(oarg), isTuple(oarg));
        goto Lnomatch;
    }
    //printf("ta is %s\n", ta->toChars());

    if (specType)
    {
        if (!ta || ta == tdummy)
            goto Lnomatch;

        //printf("\tcalling deduceType(): ta is %s, specType is %s\n", ta->toChars(), specType->toChars());
        MATCH m2 = deduceType(ta, sc, specType, parameters, dedtypes);
        if (m2 <= MATCHnomatch)
        {
            //printf("\tfailed deduceType\n");
            goto Lnomatch;
        }

        if (m2 < m)
            m = m2;
        if ((*dedtypes)[i])
        {
            Type *t = (Type *)(*dedtypes)[i];

            if (dependent && !t->equals(ta))    // Bugzilla 14357
                goto Lnomatch;

            /* This is a self-dependent parameter. For example:
             *  template X(T : T*) {}
             *  template X(T : S!T, alias S) {}
             */
            //printf("t = %s ta = %s\n", t->toChars(), ta->toChars());
            ta = t;
        }
    }
    else
    {
        if ((*dedtypes)[i])
        {
            // Must match already deduced type
            Type *t = (Type *)(*dedtypes)[i];

            if (!t->equals(ta))
            {
                //printf("t = %s ta = %s\n", t->toChars(), ta->toChars());
                goto Lnomatch;
            }
        }
        else
        {
            // So that matches with specializations are better
            m = MATCHconvert;
        }
    }
    (*dedtypes)[i] = ta;

    if (psparam)
        *psparam = new AliasDeclaration(loc, ident, ta);
    //printf("\tm = %d\n", m);
    return dependent ? MATCHexact : m;

Lnomatch:
    if (psparam)
        *psparam = NULL;
    //printf("\tm = %d\n", MATCHnomatch);
    return MATCHnomatch;
}


void TemplateTypeParameter::print(RootObject *oarg, RootObject *oded)
{
    printf(" %s\n", ident->toChars());

    Type *t  = isType(oarg);
    Type *ta = isType(oded);

    assert(ta);

    if (specType)
        printf("\tSpecialization: %s\n", specType->toChars());
    if (defaultType)
        printf("\tDefault:        %s\n", defaultType->toChars());
    printf("\tParameter:       %s\n", t ? t->toChars() : "NULL");
    printf("\tDeduced Type:   %s\n", ta->toChars());
}

void *TemplateTypeParameter::dummyArg()
{
    Type *t = specType;
    if (!t)
    {
        // Use this for alias-parameter's too (?)
        if (!tdummy)
            tdummy = new TypeIdentifier(loc, ident);
        t = tdummy;
    }
    return (void *)t;
}


RootObject *TemplateTypeParameter::specialization()
{
    return specType;
}

RootObject *TemplateTypeParameter::defaultArg(Loc, Scope *sc)
{
    Type *t = defaultType;
    if (t)
    {
        t = t->syntaxCopy();
        t = t->semantic(loc, sc);   // use the parameter loc
    }
    return t;
}

bool TemplateTypeParameter::hasDefaultArg()
{
    return defaultType != NULL;
}

/* ======================== TemplateThisParameter =========================== */

// this-parameter

TemplateThisParameter::TemplateThisParameter(Loc loc, Identifier *ident,
        Type *specType,
        Type *defaultType)
    : TemplateTypeParameter(loc, ident, specType, defaultType)
{
}

TemplateThisParameter  *TemplateThisParameter::isTemplateThisParameter()
{
    return this;
}

TemplateParameter *TemplateThisParameter::syntaxCopy()
{
    return new TemplateThisParameter(loc, ident,
        specType    ? specType->syntaxCopy()    : NULL,
        defaultType ? defaultType->syntaxCopy() : NULL);
}

/* ======================== TemplateAliasParameter ========================== */

// alias-parameter

Dsymbol *TemplateAliasParameter::sdummy = NULL;

TemplateAliasParameter::TemplateAliasParameter(Loc loc, Identifier *ident,
        Type *specType, RootObject *specAlias, RootObject *defaultAlias)
    : TemplateParameter(loc, ident)
{
    this->ident = ident;
    this->specType = specType;
    this->specAlias = specAlias;
    this->defaultAlias = defaultAlias;
}

TemplateAliasParameter *TemplateAliasParameter::isTemplateAliasParameter()
{
    return this;
}

TemplateParameter *TemplateAliasParameter::syntaxCopy()
{
    return new TemplateAliasParameter(loc, ident,
        specType ? specType->syntaxCopy() : NULL,
        objectSyntaxCopy(specAlias),
        objectSyntaxCopy(defaultAlias));
}

bool TemplateAliasParameter::declareParameter(Scope *sc)
{
    TypeIdentifier *ti = new TypeIdentifier(loc, ident);
    Declaration *ad = new AliasDeclaration(loc, ident, ti);
    return sc->insert(ad) != NULL;
}

static RootObject *aliasParameterSemantic(Loc loc, Scope *sc, RootObject *o, TemplateParameters *parameters)
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
                o = ta->semantic(loc, sc);
        }
        else if (ea)
        {
            sc = sc->startCTFE();
            ea = ::semantic(ea, sc);
            sc = sc->endCTFE();
            o = ea->ctfeInterpret();
        }
    }
    return o;
}

bool TemplateAliasParameter::semantic(Scope *sc, TemplateParameters *parameters)
{
    if (specType && !reliesOnTident(specType, parameters))
    {
        specType = specType->semantic(loc, sc);
    }
    specAlias = aliasParameterSemantic(loc, sc, specAlias, parameters);
    return !(specType  && isError(specType)) &&
           !(specAlias && isError(specAlias));
}

MATCH TemplateAliasParameter::matchArg(Scope *sc, RootObject *oarg,
        size_t i, TemplateParameters *parameters, Objects *dedtypes,
        Declaration **psparam)
{
    //printf("TemplateAliasParameter::matchArg('%s')\n", ident->toChars());
    MATCH m = MATCHexact;
    Type *ta = isType(oarg);
    RootObject *sa = ta && !ta->deco ? NULL : getDsymbol(oarg);
    Expression *ea = isExpression(oarg);
    if (ea && (ea->op == TOKthis || ea->op == TOKsuper))
        sa = ((ThisExp *)ea)->var;
    else if (ea && ea->op == TOKscope)
        sa = ((ScopeExp *)ea)->sds;
    if (sa)
    {
        if (((Dsymbol *)sa)->isAggregateDeclaration())
            m = MATCHconvert;

        /* specType means the alias must be a declaration with a type
         * that matches specType.
         */
        if (specType)
        {
            Declaration *d = ((Dsymbol *)sa)->isDeclaration();
            if (!d)
                goto Lnomatch;
            if (!d->type->equals(specType))
                goto Lnomatch;
        }
    }
    else
    {
        sa = oarg;
        if (ea)
        {
            if (specType)
            {
                if (!ea->type->equals(specType))
                    goto Lnomatch;
            }
        }
        else if (ta && ta->ty == Tinstance && !specAlias)
        {
            /* Bugzilla xxxxx: Specialized parameter should be prefeerd
             * match to the template type parameter.
             *  template X(alias a) {}                      // a == this
             *  template X(alias a : B!A, alias B, A...) {} // B!A => ta
             */
        }
        else if (sa && sa == TemplateTypeParameter::tdummy)
        {
            /* Bugzilla 2025: Aggregate Types should preferentially
             * match to the template type parameter.
             *  template X(alias a) {}  // a == this
             *  template X(T) {}        // T => sa
             */
        }
        else
            goto Lnomatch;
    }

    if (specAlias)
    {
        if (sa == sdummy)
            goto Lnomatch;
        Dsymbol *sx = isDsymbol(sa);
        if (sa != specAlias && sx)
        {
            Type *talias = isType(specAlias);
            if (!talias)
                goto Lnomatch;

            TemplateInstance *ti = sx->isTemplateInstance();
            if (!ti && sx->parent)
            {
                ti = sx->parent->isTemplateInstance();
                if (ti && ti->name != sx->ident)
                    goto Lnomatch;
            }
            if (!ti)
                goto Lnomatch;

            Type *t = new TypeInstance(Loc(), ti);
            MATCH m2 = deduceType(t, sc, talias, parameters, dedtypes);
            if (m2 <= MATCHnomatch)
                goto Lnomatch;
        }
    }
    else if ((*dedtypes)[i])
    {
        // Must match already deduced symbol
        RootObject *si = (*dedtypes)[i];
        if (!sa || si != sa)
            goto Lnomatch;
    }
    (*dedtypes)[i] = sa;

    if (psparam)
    {
        if (Dsymbol *s = isDsymbol(sa))
        {
            *psparam = new AliasDeclaration(loc, ident, s);
        }
        else if (Type *t = isType(sa))
        {
            *psparam = new AliasDeclaration(loc, ident, t);
        }
        else
        {
            assert(ea);

            // Declare manifest constant
            Initializer *init = new ExpInitializer(loc, ea);
            VarDeclaration *v = new VarDeclaration(loc, NULL, ident, init);
            v->storage_class = STCmanifest;
            v->semantic(sc);
            *psparam = v;
        }
    }
    return dependent ? MATCHexact : m;

Lnomatch:
    if (psparam)
        *psparam = NULL;
    //printf("\tm = %d\n", MATCHnomatch);
    return MATCHnomatch;
}


void TemplateAliasParameter::print(RootObject *, RootObject *oded)
{
    printf(" %s\n", ident->toChars());

    Dsymbol *sa = isDsymbol(oded);
    assert(sa);

    printf("\tParameter alias: %s\n", sa->toChars());
}

void *TemplateAliasParameter::dummyArg()
{
    RootObject *s = specAlias;
    if (!s)
    {
        if (!sdummy)
            sdummy = new Dsymbol();
        s = sdummy;
    }
    return (void*)s;
}


RootObject *TemplateAliasParameter::specialization()
{
    return specAlias;
}

RootObject *TemplateAliasParameter::defaultArg(Loc, Scope *sc)
{
    RootObject *da = defaultAlias;
    Type *ta = isType(defaultAlias);
    if (ta)
    {
       if (ta->ty == Tinstance)
       {
           // If the default arg is a template, instantiate for each type
           da = ta->syntaxCopy();
       }
    }

    RootObject *o = aliasParameterSemantic(loc, sc, da, NULL);  // use the parameter loc
    return o;
}

bool TemplateAliasParameter::hasDefaultArg()
{
    return defaultAlias != NULL;
}

/* ======================== TemplateValueParameter ========================== */

// value-parameter

AA *TemplateValueParameter::edummies = NULL;

TemplateValueParameter::TemplateValueParameter(Loc loc, Identifier *ident, Type *valType,
        Expression *specValue, Expression *defaultValue)
    : TemplateParameter(loc, ident)
{
    this->ident = ident;
    this->valType = valType;
    this->specValue = specValue;
    this->defaultValue = defaultValue;
}

TemplateValueParameter *TemplateValueParameter::isTemplateValueParameter()
{
    return this;
}

TemplateParameter *TemplateValueParameter::syntaxCopy()
{
    return new TemplateValueParameter(loc, ident,
        valType->syntaxCopy(),
        specValue    ? specValue->syntaxCopy()    : NULL,
        defaultValue ? defaultValue->syntaxCopy() : NULL);
}

bool TemplateValueParameter::declareParameter(Scope *sc)
{
    VarDeclaration *v = new VarDeclaration(loc, valType, ident, NULL);
    v->storage_class = STCtemplateparameter;
    return sc->insert(v) != NULL;
}

bool TemplateValueParameter::semantic(Scope *sc, TemplateParameters *)
{
    valType = valType->semantic(loc, sc);

    return !isError(valType);
}

MATCH TemplateValueParameter::matchArg(Scope *sc, RootObject *oarg,
        size_t i, TemplateParameters *, Objects *dedtypes, Declaration **psparam)
{
    //printf("TemplateValueParameter::matchArg('%s')\n", ident->toChars());

    MATCH m = MATCHexact;

    Expression *ei = isExpression(oarg);
    Type *vt;

    if (!ei && oarg)
    {
        Dsymbol *si = isDsymbol(oarg);
        FuncDeclaration *f = si ? si->isFuncDeclaration() : NULL;
        if (!f || !f->fbody || f->needThis())
            goto Lnomatch;

        ei = new VarExp(loc, f);
        ei = ::semantic(ei, sc);

        /* If a function is really property-like, and then
         * it's CTFEable, ei will be a literal expression.
         */
        unsigned int olderrors = global.startGagging();
        ei = resolveProperties(sc, ei);
        ei = ei->ctfeInterpret();
        if (global.endGagging(olderrors) || ei->op == TOKerror)
            goto Lnomatch;

        /* Bugzilla 14520: A property-like function can match to both
         * TemplateAlias and ValueParameter. But for template overloads,
         * it should always prefer alias parameter to be consistent
         * template match result.
         *
         *   template X(alias f) { enum X = 1; }
         *   template X(int val) { enum X = 2; }
         *   int f1() { return 0; }  // CTFEable
         *   int f2();               // body-less function is not CTFEable
         *   enum x1 = X!f1;    // should be 1
         *   enum x2 = X!f2;    // should be 1
         *
         * e.g. The x1 value must be same even if the f1 definition will be moved
         *      into di while stripping body code.
         */
        m = MATCHconvert;
    }

    if (ei && ei->op == TOKvar)
    {
        // Resolve const variables that we had skipped earlier
        ei = ei->ctfeInterpret();
    }

    //printf("\tvalType: %s, ty = %d\n", valType->toChars(), valType->ty);
    vt = valType->semantic(loc, sc);
    //printf("ei: %s, ei->type: %s\n", ei->toChars(), ei->type->toChars());
    //printf("vt = %s\n", vt->toChars());

    if (ei->type)
    {
        MATCH m2 = ei->implicitConvTo(vt);
        //printf("m: %d\n", m);
        if (m2 < m)
            m = m2;
        if (m <= MATCHnomatch)
            goto Lnomatch;
        ei = ei->implicitCastTo(sc, vt);
        ei = ei->ctfeInterpret();
    }

    if (specValue)
    {
        if (!ei || (Expression *)dmd_aaGetRvalue(edummies, (void *)ei->type) == ei)
            goto Lnomatch;

        Expression *e = specValue;

        sc = sc->startCTFE();
        e = ::semantic(e, sc);
        e = resolveProperties(sc, e);
        sc = sc->endCTFE();
        e = e->implicitCastTo(sc, vt);
        e = e->ctfeInterpret();

        ei = ei->syntaxCopy();
        sc = sc->startCTFE();
        ei = ::semantic(ei, sc);
        sc = sc->endCTFE();
        ei = ei->implicitCastTo(sc, vt);
        ei = ei->ctfeInterpret();
        //printf("\tei: %s, %s\n", ei->toChars(), ei->type->toChars());
        //printf("\te : %s, %s\n", e->toChars(), e->type->toChars());
        if (!ei->equals(e))
            goto Lnomatch;
    }
    else
    {
        if ((*dedtypes)[i])
        {
            // Must match already deduced value
            Expression *e = (Expression *)(*dedtypes)[i];

            if (!ei || !ei->equals(e))
                goto Lnomatch;
        }
    }
    (*dedtypes)[i] = ei;

    if (psparam)
    {
        Initializer *init = new ExpInitializer(loc, ei);
        Declaration *sparam = new VarDeclaration(loc, vt, ident, init);
        sparam->storage_class = STCmanifest;
        *psparam = sparam;
    }
    return dependent ? MATCHexact : m;

Lnomatch:
    //printf("\tno match\n");
    if (psparam)
        *psparam = NULL;
    return MATCHnomatch;
}


void TemplateValueParameter::print(RootObject *, RootObject *oded)
{
    printf(" %s\n", ident->toChars());

    Expression *ea = isExpression(oded);

    if (specValue)
        printf("\tSpecialization: %s\n", specValue->toChars());
    printf("\tParameter Value: %s\n", ea ? ea->toChars() : "NULL");
}

void *TemplateValueParameter::dummyArg()
{
    Expression *e = specValue;
    if (!e)
    {
        // Create a dummy value
        Expression **pe = (Expression **)dmd_aaGet(&edummies, (void *)valType);
        if (!*pe)
            *pe = valType->defaultInit();
        e = *pe;
    }
    return (void *)e;
}


RootObject *TemplateValueParameter::specialization()
{
    return specValue;
}

RootObject *TemplateValueParameter::defaultArg(Loc instLoc, Scope *sc)
{
    Expression *e = defaultValue;
    if (e)
    {
        e = e->syntaxCopy();
        if ((e = ::semantic(e, sc)) == NULL)
            return NULL;
        if ((e = resolveProperties(sc, e)) == NULL)
            return NULL;
        e = e->resolveLoc(instLoc, sc);     // use the instantiated loc
        e = e->optimize(WANTvalue);
    }
    return e;
}

bool TemplateValueParameter::hasDefaultArg()
{
    return defaultValue != NULL;
}

/* ======================== TemplateTupleParameter ========================== */

// variadic-parameter

TemplateTupleParameter::TemplateTupleParameter(Loc loc, Identifier *ident)
    : TemplateParameter(loc, ident)
{
    this->ident = ident;
}

TemplateTupleParameter *TemplateTupleParameter::isTemplateTupleParameter()
{
    return this;
}

TemplateParameter *TemplateTupleParameter::syntaxCopy()
{
    return new TemplateTupleParameter(loc, ident);
}

bool TemplateTupleParameter::declareParameter(Scope *sc)
{
    TypeIdentifier *ti = new TypeIdentifier(loc, ident);
    Declaration *ad = new AliasDeclaration(loc, ident, ti);
    return sc->insert(ad) != NULL;
}

bool TemplateTupleParameter::semantic(Scope *, TemplateParameters *)
{
    return true;
}

MATCH TemplateTupleParameter::matchArg(Loc, Scope *sc, Objects *tiargs,
        size_t i, TemplateParameters *parameters, Objects *dedtypes,
        Declaration **psparam)
{
    /* The rest of the actual arguments (tiargs[]) form the match
     * for the variadic parameter.
     */
    assert(i + 1 == dedtypes->dim);     // must be the last one
    Tuple *ovar;

    if (Tuple *u = isTuple((*dedtypes)[i]))
    {
        // It has already been deduced
        ovar = u;
    }
    else if (i + 1 == tiargs->dim && isTuple((*tiargs)[i]))
        ovar = isTuple((*tiargs)[i]);
    else
    {
        ovar = new Tuple();
        //printf("ovar = %p\n", ovar);
        if (i < tiargs->dim)
        {
            //printf("i = %d, tiargs->dim = %d\n", i, tiargs->dim);
            ovar->objects.setDim(tiargs->dim - i);
            for (size_t j = 0; j < ovar->objects.dim; j++)
                ovar->objects[j] = (*tiargs)[i + j];
        }
    }
    return matchArg(sc, ovar, i, parameters, dedtypes, psparam);
}

MATCH TemplateTupleParameter::matchArg(Scope *, RootObject *oarg,
        size_t i, TemplateParameters *, Objects *dedtypes, Declaration **psparam)
{
    //printf("TemplateTupleParameter::matchArg('%s')\n", ident->toChars());
    Tuple *ovar = isTuple(oarg);
    if (!ovar)
        return MATCHnomatch;
    if ((*dedtypes)[i])
    {
        Tuple *tup = isTuple((*dedtypes)[i]);
        if (!tup)
            return MATCHnomatch;
        if (!match(tup, ovar))
            return MATCHnomatch;
    }
    (*dedtypes)[i] = ovar;

    if (psparam)
        *psparam = new TupleDeclaration(loc, ident, &ovar->objects);
    return dependent ? MATCHexact : MATCHconvert;
}


void TemplateTupleParameter::print(RootObject *, RootObject *oded)
{
    printf(" %s... [", ident->toChars());
    Tuple *v = isTuple(oded);
    assert(v);

    //printf("|%d| ", v->objects.dim);
    for (size_t i = 0; i < v->objects.dim; i++)
    {
        if (i)
            printf(", ");

        RootObject *o = v->objects[i];

        Dsymbol *sa = isDsymbol(o);
        if (sa)
            printf("alias: %s", sa->toChars());

        Type *ta = isType(o);
        if (ta)
            printf("type: %s", ta->toChars());

        Expression *ea = isExpression(o);
        if (ea)
            printf("exp: %s", ea->toChars());

        assert(!isTuple(o));            // no nested Tuple arguments
    }

    printf("]\n");
}

void *TemplateTupleParameter::dummyArg()
{
    return NULL;
}


RootObject *TemplateTupleParameter::specialization()
{
    return NULL;
}

RootObject *TemplateTupleParameter::defaultArg(Loc, Scope *)
{
    return NULL;
}

bool TemplateTupleParameter::hasDefaultArg()
{
    return false;
}

/* ======================== TemplateInstance ================================ */

TemplateInstance::TemplateInstance(Loc loc, Identifier *ident)
    : ScopeDsymbol(NULL)
{
    this->loc = loc;
    this->name = ident;
    this->tiargs = NULL;
    this->tempdecl = NULL;
    this->inst = NULL;
    this->tinst = NULL;
    this->tnext = NULL;
    this->minst = NULL;
    this->deferred = NULL;
    this->memberOf = NULL;
    this->argsym = NULL;
    this->aliasdecl = NULL;
    this->semantictiargsdone = false;
    this->inuse = 0;
    this->nest = 0;
    this->havetempdecl = false;
    this->enclosing = NULL;
    this->gagged = false;
    this->hash = 0;
    this->fargs = NULL;
}

/*****************
 * This constructor is only called when we figured out which function
 * template to instantiate.
 */

TemplateInstance::TemplateInstance(Loc loc, TemplateDeclaration *td, Objects *tiargs)
    : ScopeDsymbol(NULL)
{
    this->loc = loc;
    this->name = td->ident;
    this->tiargs = tiargs;
    this->tempdecl = td;
    this->inst = NULL;
    this->tinst = NULL;
    this->tnext = NULL;
    this->minst = NULL;
    this->deferred = NULL;
    this->memberOf = NULL;
    this->argsym = NULL;
    this->aliasdecl = NULL;
    this->semantictiargsdone = true;
    this->inuse = 0;
    this->nest = 0;
    this->havetempdecl = true;
    this->enclosing = NULL;
    this->gagged = false;
    this->hash = 0;
    this->fargs = NULL;

    assert(tempdecl->_scope);
}


Objects *TemplateInstance::arraySyntaxCopy(Objects *objs)
{
    Objects *a = NULL;
    if (objs)
    {
        a = new Objects();
        a->setDim(objs->dim);
        for (size_t i = 0; i < objs->dim; i++)
            (*a)[i] = objectSyntaxCopy((*objs)[i]);
    }
    return a;
}

Dsymbol *TemplateInstance::syntaxCopy(Dsymbol *s)
{
    TemplateInstance *ti =
        s ? (TemplateInstance *)s
          : new TemplateInstance(loc, name);
    ti->tiargs = arraySyntaxCopy(tiargs);
    TemplateDeclaration *td;
    if (inst && tempdecl && (td = tempdecl->isTemplateDeclaration()) != NULL)
        td->ScopeDsymbol::syntaxCopy(ti);
    else
        ScopeDsymbol::syntaxCopy(ti);
    return ti;
}

void TemplateInstance::semantic(Scope *sc)
{
    semantic(sc, NULL);
}

void TemplateInstance::expandMembers(Scope *sc2)
{
    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->setScope(sc2);
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->importAll(sc2);
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        //printf("\t[%d] semantic on '%s' %p kind %s in '%s'\n", i, s->toChars(), s, s->kind(), this->toChars());
        //printf("test: enclosing = %d, sc2->parent = %s\n", enclosing, sc2->parent->toChars());
//      if (enclosing)
//          s->parent = sc->parent;
        //printf("test3: enclosing = %d, s->parent = %s\n", enclosing, s->parent->toChars());
        s->semantic(sc2);
        //printf("test4: enclosing = %d, s->parent = %s\n", enclosing, s->parent->toChars());
        Module::runDeferredSemantic();
    }
}

void TemplateInstance::tryExpandMembers(Scope *sc2)
{
    static int nest;
    // extracted to a function to allow windows SEH to work without destructors in the same function
    //printf("%d\n", nest);
    if (++nest > 500)
    {
        global.gag = 0;                 // ensure error message gets printed
        error("recursive expansion");
        fatal();
    }

    expandMembers(sc2);

    nest--;
}

void TemplateInstance::trySemantic3(Scope *sc2)
{
    // extracted to a function to allow windows SEH to work without destructors in the same function
    static int nest;
    //printf("%d\n", nest);
    if (++nest > 300)
    {
        global.gag = 0;            // ensure error message gets printed
        error("recursive expansion");
        fatal();
    }
    semantic3(sc2);

    --nest;
}

void TemplateInstance::semantic(Scope *sc, Expressions *fargs)
{
    //printf("[%s] TemplateInstance::semantic('%s', this=%p, gag = %d, sc = %p)\n", loc.toChars(), toChars(), this, global.gag, sc);
    if (inst)           // if semantic() was already run
    {
        return;
    }
    if (semanticRun != PASSinit)
    {
        Ungag ungag(global.gag);
        if (!gagged)
            global.gag = 0;
        error(loc, "recursive template expansion");
        if (gagged)
            semanticRun = PASSinit;
        else
            inst = this;
        errors = true;
        return;
    }

    // Get the enclosing template instance from the scope tinst
    tinst = sc->tinst;

    // Get the instantiating module from the scope minst
    minst = sc->minst;
    // Bugzilla 10920: If the enclosing function is non-root symbol,
    // this instance should be speculative.
    if (!tinst && sc->func && sc->func->inNonRoot())
    {
        minst = NULL;
    }

    gagged = (global.gag > 0);

    semanticRun = PASSsemantic;

    /* Find template declaration first,
     * then run semantic on each argument (place results in tiargs[]),
     * last find most specialized template from overload list/set.
     */
    if (!findTempDecl(sc, NULL) ||
        !semanticTiargs(sc) ||
        !findBestMatch(sc, fargs))
    {
Lerror:
        if (gagged)
        {
            // Bugzilla 13220: Rollback status for later semantic re-running.
            semanticRun = PASSinit;
        }
        else
            inst = this;
        errors = true;
        return;
    }
    TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
    assert(tempdecl);

    // If tempdecl is a mixin, disallow it
    if (tempdecl->ismixin)
    {
        error("mixin templates are not regular templates");
        goto Lerror;
    }

    hasNestedArgs(tiargs, tempdecl->isstatic);
    if (errors)
        goto Lerror;

    /* See if there is an existing TemplateInstantiation that already
     * implements the typeargs. If so, just refer to that one instead.
     */
    inst = tempdecl->findExistingInstance(this, fargs);
    TemplateInstance *errinst = NULL;
    if (!inst)
    {
        // So, we need to implement 'this' instance.
    }
    else if (inst->gagged && !gagged && inst->errors)
    {
        // If the first instantiation had failed, re-run semantic,
        // so that error messages are shown.
        errinst = inst;
    }
    else
    {
        // It's a match
        parent = inst->parent;
        errors = inst->errors;

        // If both this and the previous instantiation were gagged,
        // use the number of errors that happened last time.
        global.errors += errors;
        global.gaggedErrors += errors;

        // If the first instantiation was gagged, but this is not:
        if (inst->gagged)
        {
            // It had succeeded, mark it is a non-gagged instantiation,
            // and reuse it.
            inst->gagged = gagged;
        }

        this->tnext = inst->tnext;
        inst->tnext = this;

        /* A module can have explicit template instance and its alias
         * in module scope (e,g, `alias Base64 = Base64Impl!('+', '/');`).
         * If the first instantiation 'inst' had happened in non-root module,
         * compiler can assume that its instantiated code would be included
         * in the separately compiled obj/lib file (e.g. phobos.lib).
         *
         * However, if 'this' second instantiation happened in root module,
         * compiler might need to invoke its codegen (Bugzilla 2500 & 2644).
         * But whole import graph is not determined until all semantic pass finished,
         * so 'inst' should conservatively finish the semantic3 pass for the codegen.
         */
        if (minst && minst->isRoot() && !(inst->minst && inst->minst->isRoot()))
        {
            /* Swap the position of 'inst' and 'this' in the instantiation graph.
             * Then, the primary instance `inst` will be changed to a root instance.
             *
             * Before:
             *  non-root -> A!() -> B!()[inst] -> C!()
             *                      |
             *  root     -> D!() -> B!()[this]
             *
             * After:
             *  non-root -> A!() -> B!()[this]
             *                      |
             *  root     -> D!() -> B!()[inst] -> C!()
             */
            Module *mi = minst;
            TemplateInstance *ti = tinst;
            minst = inst->minst;
            tinst = inst->tinst;
            inst->minst = mi;
            inst->tinst = ti;

            if (minst)  // if inst was not speculative
            {
                /* Add 'inst' once again to the root module members[], then the
                 * instance members will get codegen chances.
                 */
                inst->appendToModuleMember();
            }
        }

        return;
    }
    unsigned errorsave = global.errors;

    inst = this;
    parent = enclosing ? enclosing : tempdecl->parent;
    //printf("parent = '%s'\n", parent->kind());

    TemplateInstance *tempdecl_instance_idx = tempdecl->addInstance(this);

    //getIdent();

    // Store the place we added it to in target_symbol_list(_idx) so we can
    // remove it later if we encounter an error.
    Dsymbols *target_symbol_list = appendToModuleMember();
    size_t target_symbol_list_idx = target_symbol_list ? target_symbol_list->dim - 1 : 0;

    // Copy the syntax trees from the TemplateDeclaration
    members = Dsymbol::arraySyntaxCopy(tempdecl->members);

    // resolve TemplateThisParameter
    for (size_t i = 0; i < tempdecl->parameters->dim; i++)
    {
        if ((*tempdecl->parameters)[i]->isTemplateThisParameter() == NULL)
            continue;
        Type *t = isType((*tiargs)[i]);
        assert(t);
        if (StorageClass stc = ModToStc(t->mod))
        {
            //printf("t = %s, stc = x%llx\n", t->toChars(), stc);
            Dsymbols *s = new Dsymbols();
            s->push(new StorageClassDeclaration(stc, members));
            members = s;
        }
        break;
    }

    // Create our own scope for the template parameters
    Scope *scope = tempdecl->_scope;
    if (tempdecl->semanticRun == PASSinit)
    {
        error("template instantiation %s forward references template declaration %s", toChars(), tempdecl->toChars());
        return;
    }

    argsym = new ScopeDsymbol();
    argsym->parent = scope->parent;
    scope = scope->push(argsym);
    scope->tinst = this;
    scope->minst = minst;
    //scope->stc = 0;

    // Declare each template parameter as an alias for the argument type
    Scope *paramscope = scope->push();
    paramscope->stc = 0;
    paramscope->protection = Prot(PROTpublic);  // Bugzilla 14169: template parameters should be public
    declareParameters(paramscope);
    paramscope->pop();

    // Add members of template instance to template instance symbol table
//    parent = scope->scopesym;
    symtab = new DsymbolTable();
    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->addMember(scope, this);
    }

    /* See if there is only one member of template instance, and that
     * member has the same name as the template instance.
     * If so, this template instance becomes an alias for that member.
     */
    //printf("members->dim = %d\n", members->dim);
    if (members->dim)
    {
        Dsymbol *s;
        if (Dsymbol::oneMembers(members, &s, tempdecl->ident) && s)
        {
            //printf("tempdecl->ident = %s, s = '%s'\n", tempdecl->ident->toChars(), s->kind(), s->toPrettyChars());
            //printf("setting aliasdecl\n");
            aliasdecl = s;
        }
    }

    /* If function template declaration
     */
    if (fargs && aliasdecl)
    {
        FuncDeclaration *fd = aliasdecl->isFuncDeclaration();
        if (fd)
        {
            /* Transmit fargs to type so that TypeFunction::semantic() can
             * resolve any "auto ref" storage classes.
             */
            TypeFunction *tf = (TypeFunction *)fd->type;
            if (tf && tf->ty == Tfunction)
                tf->fargs = fargs;
        }
    }

    // Do semantic() analysis on template instance members
    Scope *sc2;
    sc2 = scope->push(this);
    //printf("enclosing = %d, sc->parent = %s\n", enclosing, sc->parent->toChars());
    sc2->parent = this;
    sc2->tinst = this;
    sc2->minst = minst;

    tryExpandMembers(sc2);

    semanticRun = PASSsemanticdone;

    /* ConditionalDeclaration may introduce eponymous declaration,
     * so we should find it once again after semantic.
     */
    if (members->dim)
    {
        Dsymbol *s;
        if (Dsymbol::oneMembers(members, &s, tempdecl->ident) && s)
        {
            if (!aliasdecl || aliasdecl != s)
            {
                //printf("tempdecl->ident = %s, s = '%s'\n", tempdecl->ident->toChars(), s->kind(), s->toPrettyChars());
                //printf("setting aliasdecl 2\n");
                aliasdecl = s;
            }
        }
    }

    if (global.errors != errorsave)
        goto Laftersemantic;

    /* If any of the instantiation members didn't get semantic() run
     * on them due to forward references, we cannot run semantic2()
     * or semantic3() yet.
     */
    {
    bool found_deferred_ad = false;
    for (size_t i = 0; i < Module::deferred.dim; i++)
    {
        Dsymbol *sd = Module::deferred[i];
        AggregateDeclaration *ad = sd->isAggregateDeclaration();
        if (ad && ad->parent && ad->parent->isTemplateInstance())
        {
            //printf("deferred template aggregate: %s %s\n",
            //        sd->parent->toChars(), sd->toChars());
            found_deferred_ad = true;
            if (ad->parent == this)
            {
                ad->deferred = this;
                break;
            }
        }
    }
    if (found_deferred_ad || Module::deferred.dim)
        goto Laftersemantic;
    }

    /* The problem is when to parse the initializer for a variable.
     * Perhaps VarDeclaration::semantic() should do it like it does
     * for initializers inside a function.
     */
    //if (sc->parent->isFuncDeclaration())
    {
        /* BUG 782: this has problems if the classes this depends on
         * are forward referenced. Find a way to defer semantic()
         * on this template.
         */
        semantic2(sc2);
    }
    if (global.errors != errorsave)
        goto Laftersemantic;

    if ((sc->func || (sc->flags & SCOPEfullinst)) && !tinst)
    {
        /* If a template is instantiated inside function, the whole instantiation
         * should be done at that position. But, immediate running semantic3 of
         * dependent templates may cause unresolved forward reference (Bugzilla 9050).
         * To avoid the issue, don't run semantic3 until semantic and semantic2 done.
         */
        TemplateInstances deferred;
        this->deferred = &deferred;

        //printf("Run semantic3 on %s\n", toChars());
        trySemantic3(sc2);

        for (size_t i = 0; i < deferred.dim; i++)
        {
            //printf("+ run deferred semantic3 on %s\n", deferred[i]->toChars());
            deferred[i]->semantic3(NULL);
        }

        this->deferred = NULL;
    }
    else if (tinst)
    {
        bool doSemantic3 = false;
        if (sc->func && aliasdecl && aliasdecl->toAlias()->isFuncDeclaration())
        {
            /* Template function instantiation should run semantic3 immediately
             * for attribute inference.
             */
            trySemantic3(sc2);
        }
        else if (sc->func)
        {
            /* A lambda function in template arguments might capture the
             * instantiated scope context. For the correct context inference,
             * all instantiated functions should run the semantic3 immediately.
             * See also compilable/test14973.d
             */
            for (size_t i = 0; i < tdtypes.dim; i++)
            {
                RootObject *oarg = tdtypes[i];
                Dsymbol *s = getDsymbol(oarg);
                if (!s)
                    continue;

                if (TemplateDeclaration *td = s->isTemplateDeclaration())
                {
                    if (!td->literal)
                        continue;
                    assert(td->members && td->members->dim == 1);
                    s = (*td->members)[0];
                }
                if (FuncLiteralDeclaration *fld = s->isFuncLiteralDeclaration())
                {
                    if (fld->tok == TOKreserved)
                    {
                        doSemantic3 = true;
                        break;
                    }
                }
            }
            //printf("[%s] %s doSemantic3 = %d\n", loc.toChars(), toChars(), doSemantic3);
        }
        if (doSemantic3)
            trySemantic3(sc2);

        TemplateInstance *ti = tinst;
        int nest = 0;
        while (ti && !ti->deferred && ti->tinst)
        {
            ti = ti->tinst;
            if (++nest > 500)
            {
                global.gag = 0;            // ensure error message gets printed
                error("recursive expansion");
                fatal();
            }
        }
        if (ti && ti->deferred)
        {
            //printf("deferred semantic3 of %p %s, ti = %s, ti->deferred = %p\n", this, toChars(), ti->toChars());
            for (size_t i = 0; ; i++)
            {
                if (i == ti->deferred->dim)
                {
                    ti->deferred->push(this);
                    break;
                }
                if ((*ti->deferred)[i] == this)
                    break;
            }
        }
    }

    if (aliasdecl)
    {
        /* Bugzilla 13816: AliasDeclaration tries to resolve forward reference
         * twice (See inuse check in AliasDeclaration::toAlias()). It's
         * necessary to resolve mutual references of instantiated symbols, but
         * it will left a true recursive alias in tuple declaration - an
         * AliasDeclaration A refers TupleDeclaration B, and B contains A
         * in its elements.  To correctly make it an error, we strictly need to
         * resolve the alias of eponymous member.
         */
        aliasdecl = aliasdecl->toAlias2();
    }

  Laftersemantic:
    sc2->pop();

    scope->pop();

    // Give additional context info if error occurred during instantiation
    if (global.errors != errorsave)
    {
        if (!errors)
        {
            if (!tempdecl->literal)
                error(loc, "error instantiating");
            if (tinst)
                tinst->printInstantiationTrace();
        }
        errors = true;
        if (gagged)
        {
            // Errors are gagged, so remove the template instance from the
            // instance/symbol lists we added it to and reset our state to
            // finish clean and so we can try to instantiate it again later
            // (see bugzilla 4302 and 6602).
            tempdecl->removeInstance(tempdecl_instance_idx);
            if (target_symbol_list)
            {
                // Because we added 'this' in the last position above, we
                // should be able to remove it without messing other indices up.
                assert((*target_symbol_list)[target_symbol_list_idx] == this);
                target_symbol_list->remove(target_symbol_list_idx);
                memberOf = NULL;                    // no longer a member
            }
            semanticRun = PASSinit;
            inst = NULL;
            symtab = NULL;
        }
    }
    else if (errinst)
    {
        /* Bugzilla 14541: If the previous gagged instance had failed by
         * circular references, currrent "error reproduction instantiation"
         * might succeed, because of the difference of instantiated context.
         * On such case, the cached error instance needs to be overridden by the
         * succeeded instance.
         */
        //printf("replaceInstance()\n");
        TemplateInstances *tinstances = (TemplateInstances *)dmd_aaGetRvalue((AA *)tempdecl->instances, (void *)hash);
        assert(tinstances);
        for (size_t i = 0; i < tinstances->dim; i++)
        {
            TemplateInstance *ti = (*tinstances)[i];
            if (ti == errinst)
            {
                (*tinstances)[i] = this;     // override
                break;
            }
        }
    }
}


/**********************************************
 * Find template declaration corresponding to template instance.
 *
 * Returns:
 *      false if finding fails.
 * Note:
 *      This function is reentrant against error occurrence. If returns false,
 *      any members of this object won't be modified, and repetition call will
 *      reproduce same error.
 */

bool TemplateInstance::findTempDecl(Scope *sc, WithScopeSymbol **pwithsym)
{
    if (pwithsym)
        *pwithsym = NULL;

    if (havetempdecl)
        return true;

    //printf("TemplateInstance::findTempDecl() %s\n", toChars());
    if (!tempdecl)
    {
        /* Given:
         *    foo!( ... )
         * figure out which TemplateDeclaration foo refers to.
         */
        Identifier *id = name;
        Dsymbol *scopesym;
        Dsymbol *s = sc->search(loc, id, &scopesym);
        if (!s)
        {
            s = sc->search_correct(id);
            if (s)
                error("template '%s' is not defined, did you mean %s?", id->toChars(), s->toChars());
            else
                error("template '%s' is not defined", id->toChars());
            return false;
        }

        if (pwithsym)
            *pwithsym = scopesym->isWithScopeSymbol();

        /* We might have found an alias within a template when
         * we really want the template.
         */
        TemplateInstance *ti;
        if (s->parent &&
            (ti = s->parent->isTemplateInstance()) != NULL)
        {
            if (ti->tempdecl && ti->tempdecl->ident == id)
            {
                /* This is so that one can refer to the enclosing
                 * template, even if it has the same name as a member
                 * of the template, if it has a !(arguments)
                 */
                TemplateDeclaration *td = ti->tempdecl->isTemplateDeclaration();
                assert(td);
                if (td->overroot)       // if not start of overloaded list of TemplateDeclaration's
                    td = td->overroot;  // then get the start
                s = td;
            }
        }

        if (!updateTempDecl(sc, s))
        {
            return false;
        }
    }
    assert(tempdecl);

  struct ParamFwdTi
  {
    static int fp(void *param, Dsymbol *s)
    {
        TemplateDeclaration *td = s->isTemplateDeclaration();
        if (!td)
            return 0;

        TemplateInstance *ti = (TemplateInstance *)param;
        if (td->semanticRun == PASSinit)
        {
            if (td->_scope)
            {
                // Try to fix forward reference. Ungag errors while doing so.
                Ungag ungag = td->ungagSpeculative();
                td->semantic(td->_scope);
            }
            if (td->semanticRun == PASSinit)
            {
                ti->error("%s forward references template declaration %s", ti->toChars(), td->toChars());
                return 1;
            }
        }
        return 0;
    }
  };
    // Look for forward references
    OverloadSet *tovers = tempdecl->isOverloadSet();
    size_t overs_dim = tovers ? tovers->a.dim : 1;
    for (size_t oi = 0; oi < overs_dim; oi++)
    {
        if (overloadApply(tovers ? tovers->a[oi] : tempdecl, (void *)this, &ParamFwdTi::fp))
            return false;
    }
    return true;
}

/**********************************************
 * Confirm s is a valid template, then store it.
 * Input:
 *      sc
 *      s   candidate symbol of template. It may be:
 *          TemplateDeclaration
 *          FuncDeclaration with findTemplateDeclRoot() != NULL
 *          OverloadSet which contains candidates
 * Returns:
 *      true if updating succeeds.
 */

bool TemplateInstance::updateTempDecl(Scope *sc, Dsymbol *s)
{
    if (s)
    {
        Identifier *id = name;
        s = s->toAlias();

        /* If an OverloadSet, look for a unique member that is a template declaration
         */
        OverloadSet *os = s->isOverloadSet();
        if (os)
        {
            s = NULL;
            for (size_t i = 0; i < os->a.dim; i++)
            {
                Dsymbol *s2 = os->a[i];
                if (FuncDeclaration *f = s2->isFuncDeclaration())
                    s2 = f->findTemplateDeclRoot();
                else
                    s2 = s2->isTemplateDeclaration();
                if (s2)
                {
                    if (s)
                    {
                        tempdecl = os;
                        return true;
                    }
                    s = s2;
                }
            }
            if (!s)
            {
                error("template '%s' is not defined", id->toChars());
                return false;
            }
        }

        OverDeclaration *od = s->isOverDeclaration();
        if (od)
        {
            tempdecl = od;  // TODO: more strict check
            return true;
        }

        /* It should be a TemplateDeclaration, not some other symbol
         */
        if (FuncDeclaration *f = s->isFuncDeclaration())
            tempdecl = f->findTemplateDeclRoot();
        else
            tempdecl = s->isTemplateDeclaration();
        if (!tempdecl)
        {
            if (!s->parent && global.errors)
                return false;
            if (!s->parent && s->getType())
            {
                Dsymbol *s2 = s->getType()->toDsymbol(sc);
                if (!s2)
                {
                    error("%s is not a template declaration, it is a %s", id->toChars(), s->kind());
                    return false;
                }
                s = s2;
            }
            //assert(s->parent);
            TemplateInstance *ti = s->parent ? s->parent->isTemplateInstance() : NULL;
            if (ti &&
                (ti->name == s->ident ||
                 ti->toAlias()->ident == s->ident)
                &&
                ti->tempdecl)
            {
                /* This is so that one can refer to the enclosing
                 * template, even if it has the same name as a member
                 * of the template, if it has a !(arguments)
                 */
                TemplateDeclaration *td = ti->tempdecl->isTemplateDeclaration();
                assert(td);
                if (td->overroot)       // if not start of overloaded list of TemplateDeclaration's
                    td = td->overroot;  // then get the start
                tempdecl = td;
            }
            else
            {
                error("%s is not a template declaration, it is a %s", id->toChars(), s->kind());
                return false;
            }
        }
    }
    return (tempdecl != NULL);
}

/**********************************
 * Run semantic on the elements of tiargs.
 * Input:
 *      sc
 * Returns:
 *      false if one or more arguments have errors.
 * Note:
 *      This function is reentrant against error occurrence. If returns false,
 *      all elements of tiargs won't be modified.
 */

bool TemplateInstance::semanticTiargs(Scope *sc)
{
    //printf("+TemplateInstance::semanticTiargs() %s\n", toChars());
    if (semantictiargsdone)
        return true;
    if (semanticTiargs(loc, sc, tiargs, 0))
    {
        // cache the result iff semantic analysis succeeded entirely
        semantictiargsdone = 1;
        return true;
    }
    return false;
}

/**********************************
 * Run semantic of tiargs as arguments of template.
 * Input:
 *      loc
 *      sc
 *      tiargs  array of template arguments
 *      flags   1: replace const variables with their initializers
 *              2: don't devolve Parameter to Type
 * Returns:
 *      false if one or more arguments have errors.
 */

bool TemplateInstance::semanticTiargs(Loc loc, Scope *sc, Objects *tiargs, int flags)
{
    // Run semantic on each argument, place results in tiargs[]
    //printf("+TemplateInstance::semanticTiargs()\n");
    if (!tiargs)
        return true;
    bool err = false;
    for (size_t j = 0; j < tiargs->dim; j++)
    {
        RootObject *o = (*tiargs)[j];
        Type *ta = isType(o);
        Expression *ea = isExpression(o);
        Dsymbol *sa = isDsymbol(o);

        //printf("1: (*tiargs)[%d] = %p, s=%p, v=%p, ea=%p, ta=%p\n", j, o, isDsymbol(o), isTuple(o), ea, ta);
        if (ta)
        {
            //printf("type %s\n", ta->toChars());
            // It might really be an Expression or an Alias
            ta->resolve(loc, sc, &ea, &ta, &sa);
            if (ea) goto Lexpr;
            if (sa) goto Ldsym;
            if (ta == NULL)
            {
                assert(global.errors);
                ta = Type::terror;
            }

        Ltype:
            if (ta->ty == Ttuple)
            {
                // Expand tuple
                TypeTuple *tt = (TypeTuple *)ta;
                size_t dim = tt->arguments->dim;
                tiargs->remove(j);
                if (dim)
                {
                    tiargs->reserve(dim);
                    for (size_t i = 0; i < dim; i++)
                    {
                        Parameter *arg = (*tt->arguments)[i];
                        if (flags & 2 && arg->ident)
                            tiargs->insert(j + i, arg);
                        else
                            tiargs->insert(j + i, arg->type);
                    }
                }
                j--;
                continue;
            }
            if (ta->ty == Terror)
            {
                err = true;
                continue;
            }
            (*tiargs)[j] = ta->merge2();
        }
        else if (ea)
        {
        Lexpr:
            //printf("+[%d] ea = %s %s\n", j, Token::toChars(ea->op), ea->toChars());
            if (flags & 1) // only used by __traits
            {
                ea = ::semantic(ea, sc);

                // must not interpret the args, excepting template parameters
                if (ea->op != TOKvar ||
                    (((VarExp *)ea)->var->storage_class & STCtemplateparameter))
                {
                    ea = ea->optimize(WANTvalue);
                }
            }
            else
            {
                sc = sc->startCTFE();
                ea = ::semantic(ea, sc);
                sc = sc->endCTFE();

                if (ea->op == TOKvar)
                {
                    /* This test is to skip substituting a const var with
                     * its initializer. The problem is the initializer won't
                     * match with an 'alias' parameter. Instead, do the
                     * const substitution in TemplateValueParameter::matchArg().
                     */
                }
                else if (definitelyValueParameter(ea))
                {
                    if (ea->checkValue())   // check void expression
                        ea = new ErrorExp();
                    unsigned int olderrs = global.errors;
                    ea = ea->ctfeInterpret();
                    if (global.errors != olderrs)
                        ea = new ErrorExp();
                }
            }
            //printf("-[%d] ea = %s %s\n", j, Token::toChars(ea->op), ea->toChars());
            if (ea->op == TOKtuple)
            {
                // Expand tuple
                TupleExp *te = (TupleExp *)ea;
                size_t dim = te->exps->dim;
                tiargs->remove(j);
                if (dim)
                {
                    tiargs->reserve(dim);
                    for (size_t i = 0; i < dim; i++)
                        tiargs->insert(j + i, (*te->exps)[i]);
                }
                j--;
                continue;
            }
            if (ea->op == TOKerror)
            {
                err = true;
                continue;
            }
            (*tiargs)[j] = ea;

            if (ea->op == TOKtype)
            {
                ta = ea->type;
                goto Ltype;
            }
            if (ea->op == TOKscope)
            {
                sa = ((ScopeExp *)ea)->sds;
                goto Ldsym;
            }
            if (ea->op == TOKfunction)
            {
                FuncExp *fe = (FuncExp *)ea;
                /* A function literal, that is passed to template and
                 * already semanticed as function pointer, never requires
                 * outer frame. So convert it to global function is valid.
                 */
                if (fe->fd->tok == TOKreserved && fe->type->ty == Tpointer)
                {
                    // change to non-nested
                    fe->fd->tok = TOKfunction;
                    fe->fd->vthis = NULL;
                }
                else if (fe->td)
                {
                    /* If template argument is a template lambda,
                     * get template declaration itself. */
                    //sa = fe->td;
                    //goto Ldsym;
                }
            }
            if (ea->op == TOKdotvar)
            {
                // translate expression to dsymbol.
                sa = ((DotVarExp *)ea)->var;
                goto Ldsym;
            }
            if (ea->op == TOKtemplate)
            {
                sa = ((TemplateExp *)ea)->td;
                goto Ldsym;
            }
            if (ea->op == TOKdottd)
            {
                // translate expression to dsymbol.
                sa = ((DotTemplateExp *)ea)->td;
                goto Ldsym;
            }
        }
        else if (sa)
        {
        Ldsym:
            //printf("dsym %s %s\n", sa->kind(), sa->toChars());
            if (sa->errors)
            {
                err = true;
                continue;
            }

            TupleDeclaration *d = sa->toAlias()->isTupleDeclaration();
            if (d)
            {
                // Expand tuple
                tiargs->remove(j);
                tiargs->insert(j, d->objects);
                j--;
                continue;
            }
            if (FuncAliasDeclaration *fa = sa->isFuncAliasDeclaration())
            {
                FuncDeclaration *f = fa->toAliasFunc();
                if (!fa->hasOverloads && f->isUnique())
                {
                    // Strip FuncAlias only when the aliased function
                    // does not have any overloads.
                    sa = f;
                }
            }
            (*tiargs)[j] = sa;

            TemplateDeclaration *td = sa->isTemplateDeclaration();
            if (td && td->semanticRun == PASSinit && td->literal)
            {
                td->semantic(sc);
            }
            FuncDeclaration *fd = sa->isFuncDeclaration();
            if (fd)
                fd->functionSemantic();
        }
        else if (isParameter(o))
        {
        }
        else
        {
            assert(0);
        }
        //printf("1: (*tiargs)[%d] = %p\n", j, (*tiargs)[j]);
    }
    return !err;
}

bool TemplateInstance::findBestMatch(Scope *sc, Expressions *fargs)
{
    if (havetempdecl)
    {
        TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
        assert(tempdecl);
        assert(tempdecl->_scope);
        // Deduce tdtypes
        tdtypes.setDim(tempdecl->parameters->dim);
        if (!tempdecl->matchWithInstance(sc, this, &tdtypes, fargs, 2))
        {
            error("incompatible arguments for template instantiation");
            return false;
        }
        // TODO: Normalizing tiargs for bugzilla 7469 is necessary?
        return true;
    }

    unsigned errs = global.errors;

  struct ParamBest
  {
    // context
    Scope *sc;
    TemplateInstance *ti;
    Objects dedtypes;
    // result
    TemplateDeclaration *td_best;
    TemplateDeclaration *td_ambig;
    MATCH m_best;

    static int fp(void *param, Dsymbol *s)
    {
        return ((ParamBest *)param)->fp(s);
    }
    int fp(Dsymbol *s)
    {
        TemplateDeclaration *td = s->isTemplateDeclaration();
        if (!td)
            return 0;

        if (td == td_best)          // skip duplicates
            return 0;

        //printf("td = %s\n", td->toPrettyChars());

        // If more arguments than parameters,
        // then this is no match.
        if (td->parameters->dim < ti->tiargs->dim)
        {
            if (!td->isVariadic())
                return 0;
        }

        dedtypes.setDim(td->parameters->dim);
        dedtypes.zero();
        assert(td->semanticRun != PASSinit);
        MATCH m = td->matchWithInstance(sc, ti, &dedtypes, ti->fargs, 0);
        //printf("matchWithInstance = %d\n", m);
        if (m <= MATCHnomatch)                 // no match at all
            return 0;

        if (m < m_best) goto Ltd_best;
        if (m > m_best) goto Ltd;

        {
        // Disambiguate by picking the most specialized TemplateDeclaration
        MATCH c1 = td->leastAsSpecialized(sc, td_best, ti->fargs);
        MATCH c2 = td_best->leastAsSpecialized(sc, td, ti->fargs);
        //printf("c1 = %d, c2 = %d\n", c1, c2);
        if (c1 > c2) goto Ltd;
        if (c1 < c2) goto Ltd_best;
        }

        td_ambig = td;
        return 0;

      Ltd_best:         // td_best is the best match so far
        td_ambig = NULL;
        return 0;

      Ltd:              // td is the new best match
        td_ambig = NULL;
        td_best = td;
        m_best = m;
        ti->tdtypes.setDim(dedtypes.dim);
        memcpy(ti->tdtypes.tdata(), dedtypes.tdata(), ti->tdtypes.dim * sizeof(void *));
        return 0;
    }
  };
    ParamBest p;
    // context
    p.ti = this;
    p.sc = sc;

    /* Since there can be multiple TemplateDeclaration's with the same
     * name, look for the best match.
     */
    TemplateDeclaration *td_last = NULL;

    OverloadSet *tovers = tempdecl->isOverloadSet();
    size_t overs_dim = tovers ? tovers->a.dim : 1;
    for (size_t oi = 0; oi < overs_dim; oi++)
    {
        // result
        p.td_best  = NULL;
        p.td_ambig = NULL;
        p.m_best   = MATCHnomatch;
        overloadApply(tovers ? tovers->a[oi] : tempdecl, &p, &ParamBest::fp);

        if (p.td_ambig)
        {
            ::error(loc, "%s %s.%s matches more than one template declaration:\n%s:     %s\nand\n%s:     %s",
                    p.td_best->kind(), p.td_best->parent->toPrettyChars(), p.td_best->ident->toChars(),
                    p.td_best->loc.toChars() , p.td_best->toChars(),
                    p.td_ambig->loc.toChars(), p.td_ambig->toChars());
            return false;
        }
        if (p.td_best)
        {
            if (!td_last)
                td_last = p.td_best;
            else if (td_last != p.td_best)
            {
                ScopeDsymbol::multiplyDefined(loc, td_last, p.td_best);
                return false;
            }
        }
    }

    if (td_last)
    {
        /* Bugzilla 7469: Normalize tiargs by using corresponding deduced
         * template value parameters and tuples for the correct mangling.
         *
         * By doing this before hasNestedArgs, CTFEable local variable will be
         * accepted as a value parameter. For example:
         *
         *  void foo() {
         *    struct S(int n) {}   // non-global template
         *    const int num = 1;   // CTFEable local variable
         *    S!num s;             // S!1 is instantiated, not S!num
         *  }
         */
        size_t dim = td_last->parameters->dim - (td_last->isVariadic() ? 1 : 0);
        for (size_t i = 0; i < dim; i++)
        {
            if (tiargs->dim <= i)
                tiargs->push(tdtypes[i]);
            assert(i < tiargs->dim);

            TemplateValueParameter *tvp = (*td_last->parameters)[i]->isTemplateValueParameter();
            if (!tvp)
                continue;
            assert(tdtypes[i]);
            // tdtypes[i] is already normalized to the required type in matchArg

            (*tiargs)[i] = tdtypes[i];
        }
        if (td_last->isVariadic() && tiargs->dim == dim && tdtypes[dim])
        {
            Tuple *va = isTuple(tdtypes[dim]);
            assert(va);
            for (size_t i = 0; i < va->objects.dim; i++)
                tiargs->push(va->objects[i]);
        }
    }
    else if (errors && inst)
    {
        // instantiation was failed with error reporting
        assert(global.errors);
        return false;
    }
    else
    {
        TemplateDeclaration *tdecl = tempdecl->isTemplateDeclaration();

        if (errs != global.errors)
            errorSupplemental(loc, "while looking for match for %s", toChars());
        else if (tdecl && !tdecl->overnext)
        {
            // Only one template, so we can give better error message
            error("does not match template declaration %s", tdecl->toChars());
        }
        else
            ::error(loc, "%s %s.%s does not match any template declaration",
                    tempdecl->kind(), tempdecl->parent->toPrettyChars(), tempdecl->ident->toChars());
        return false;
    }

    /* The best match is td_last
     */
    tempdecl = td_last;

    return (errs == global.errors);
}

/*****************************************************
 * Determine if template instance is really a template function,
 * and that template function needs to infer types from the function
 * arguments.
 *
 * Like findBestMatch, iterate possible template candidates,
 * but just looks only the necessity of type inference.
 */

bool TemplateInstance::needsTypeInference(Scope *sc, int flag)
{
    //printf("TemplateInstance::needsTypeInference() %s\n", toChars());
    if (semanticRun != PASSinit)
        return false;

  struct ParamNeedsInf
  {
    // context
    Scope *sc;
    TemplateInstance *ti;
    int flag;
    // result
    Objects dedtypes;
    size_t count;

    static int fp(void *param, Dsymbol *s)
    {
        return ((ParamNeedsInf *)param)->fp(s);
    }
    int fp(Dsymbol *s)
    {
        TemplateDeclaration *td = s->isTemplateDeclaration();
        if (!td)
        {
            return 0;
        }

        /* If any of the overloaded template declarations need inference,
         * then return true
         */
        FuncDeclaration *fd;
        if (!td->onemember)
            return 0;
        if (TemplateDeclaration *td2 = td->onemember->isTemplateDeclaration())
        {
            if (!td2->onemember || !td2->onemember->isFuncDeclaration())
                return 0;
            if (ti->tiargs->dim >= td->parameters->dim - (td->isVariadic() ? 1 : 0))
                return 0;
            return 1;
        }
        if ((fd = td->onemember->isFuncDeclaration()) == NULL ||
            fd->type->ty != Tfunction)
        {
            return 0;
        }

        for (size_t i = 0; i < td->parameters->dim; i++)
        {
            if ((*td->parameters)[i]->isTemplateThisParameter())
                return 1;
        }

        /* Determine if the instance arguments, tiargs, are all that is necessary
         * to instantiate the template.
         */
        //printf("tp = %p, td->parameters->dim = %d, tiargs->dim = %d\n", tp, td->parameters->dim, ti->tiargs->dim);
        TypeFunction *tf = (TypeFunction *)fd->type;
        if (size_t dim = Parameter::dim(tf->parameters))
        {
            TemplateParameter *tp = td->isVariadic();
            if (tp && td->parameters->dim > 1)
                return 1;

            if (!tp && ti->tiargs->dim < td->parameters->dim)
            {
                // Can remain tiargs be filled by default arguments?
                for (size_t i = ti->tiargs->dim; i < td->parameters->dim; i++)
                {
                    if (!(*td->parameters)[i]->hasDefaultArg())
                        return 1;
                }
            }

            for (size_t i = 0; i < dim; i++)
            {
                // 'auto ref' needs inference.
                if (Parameter::getNth(tf->parameters, i)->storageClass & STCauto)
                    return 1;
            }
        }

        if (!flag)
        {
            /* Calculate the need for overload resolution.
             * When only one template can match with tiargs, inference is not necessary.
             */
            dedtypes.setDim(td->parameters->dim);
            dedtypes.zero();
            if (td->semanticRun == PASSinit)
            {
                if (td->_scope)
                {
                    // Try to fix forward reference. Ungag errors while doing so.
                    Ungag ungag = td->ungagSpeculative();
                    td->semantic(td->_scope);
                }
                if (td->semanticRun == PASSinit)
                {
                    ti->error("%s forward references template declaration %s", ti->toChars(), td->toChars());
                    return 1;
                }
            }
            assert(td->semanticRun != PASSinit);
            MATCH m = td->matchWithInstance(sc, ti, &dedtypes, NULL, 0);
            if (m <= MATCHnomatch)
                return 0;
        }

        /* If there is more than one function template which matches, we may
         * need type inference (see Bugzilla 4430)
         */
        if (++count > 1)
            return 1;

        return 0;
    }
  };
    ParamNeedsInf p;
    // context
    p.ti    = this;
    p.sc    = sc;
    p.flag  = flag;
    // result
    p.count = 0;

    OverloadSet *tovers = tempdecl->isOverloadSet();
    size_t overs_dim = tovers ? tovers->a.dim : 1;
    unsigned olderrs = global.errors;
    for (size_t oi = 0; oi < overs_dim; oi++)
    {
        if (overloadApply(tovers ? tovers->a[oi] : tempdecl, &p, &ParamNeedsInf::fp))
            return true;
    }
    if (olderrs != global.errors)
    {
        if (!global.gag)
        {
            errorSupplemental(loc, "while looking for match for %s", toChars());
            semanticRun = PASSsemanticdone;
            inst = this;
        }
        errors = true;
    }
    //printf("false\n");
    return false;
}


/*****************************************
 * Determines if a TemplateInstance will need a nested
 * generation of the TemplateDeclaration.
 * Sets enclosing property if so, and returns != 0;
 */

bool TemplateInstance::hasNestedArgs(Objects *args, bool isstatic)
{
    int nested = 0;
    //printf("TemplateInstance::hasNestedArgs('%s')\n", tempdecl->ident->toChars());

    /* A nested instance happens when an argument references a local
     * symbol that is on the stack.
     */
    for (size_t i = 0; i < args->dim; i++)
    {
        RootObject *o = (*args)[i];
        Expression *ea = isExpression(o);
        Dsymbol *sa = isDsymbol(o);
        Tuple *va = isTuple(o);
        if (ea)
        {
            if (ea->op == TOKvar)
            {
                sa = ((VarExp *)ea)->var;
                goto Lsa;
            }
            if (ea->op == TOKthis)
            {
                sa = ((ThisExp *)ea)->var;
                goto Lsa;
            }
            if (ea->op == TOKfunction)
            {
                if (((FuncExp *)ea)->td)
                    sa = ((FuncExp *)ea)->td;
                else
                    sa = ((FuncExp *)ea)->fd;
                goto Lsa;
            }
            // Emulate Expression::toMangleBuffer call that had exist in TemplateInstance::genIdent.
            if (ea->op != TOKint64 &&
                ea->op != TOKfloat64 &&
                ea->op != TOKcomplex80 &&
                ea->op != TOKnull &&
                ea->op != TOKstring &&
                ea->op != TOKarrayliteral &&
                ea->op != TOKassocarrayliteral &&
                ea->op != TOKstructliteral)
            {
                ea->error("expression %s is not a valid template value argument", ea->toChars());
                errors = true;
            }
        }
        else if (sa)
        {
          Lsa:
            sa = sa->toAlias();
            TemplateDeclaration *td = sa->isTemplateDeclaration();
            if (td)
            {
                TemplateInstance *ti = sa->toParent()->isTemplateInstance();
                if (ti && ti->enclosing)
                    sa = ti;
            }
            TemplateInstance *ti = sa->isTemplateInstance();
            Declaration *d = sa->isDeclaration();
            if ((td && td->literal) ||
                (ti && ti->enclosing) ||
                (d && !d->isDataseg() &&
                 !(d->storage_class & STCmanifest) &&
                 (!d->isFuncDeclaration() || d->isFuncDeclaration()->isNested()) &&
                 !isTemplateMixin()
                ))
            {
                // if module level template
                if (isstatic)
                {
                    Dsymbol *dparent = sa->toParent2();
                    if (!enclosing)
                        enclosing = dparent;
                    else if (enclosing != dparent)
                    {
                        /* Select the more deeply nested of the two.
                         * Error if one is not nested inside the other.
                         */
                        for (Dsymbol *p = enclosing; p; p = p->parent)
                        {
                            if (p == dparent)
                                goto L1;        // enclosing is most nested
                        }
                        for (Dsymbol *p = dparent; p; p = p->parent)
                        {
                            if (p == enclosing)
                            {
                                enclosing = dparent;
                                goto L1;        // dparent is most nested
                            }
                        }
                        error("%s is nested in both %s and %s",
                                toChars(), enclosing->toChars(), dparent->toChars());
                        errors = true;
                    }
                  L1:
                    //printf("\tnested inside %s\n", enclosing->toChars());
                    nested |= 1;
                }
                else
                {
                    error("cannot use local '%s' as parameter to non-global template %s", sa->toChars(), tempdecl->toChars());
                    errors = true;
                }
            }
        }
        else if (va)
        {
            nested |= (int)hasNestedArgs(&va->objects, isstatic);
        }
    }
    //printf("-TemplateInstance::hasNestedArgs('%s') = %d\n", tempdecl->ident->toChars(), nested);
    return nested != 0;
}

/*****************************************
 * Append 'this' to the specific module members[]
 */
Dsymbols *TemplateInstance::appendToModuleMember()
{
    Module *mi = minst;     // instantiated -> inserted module

    if (global.params.useUnitTests ||
        global.params.debuglevel)
    {
        // Turn all non-root instances to speculative
        if (mi && !mi->isRoot())
            mi = NULL;
    }

    //printf("%s->appendToModuleMember() enclosing = %s mi = %s\n",
    //    toPrettyChars(),
    //    enclosing ? enclosing->toPrettyChars() : NULL,
    //    mi ? mi->toPrettyChars() : NULL);
    if (!mi || mi->isRoot())
    {
        /* If the instantiated module is speculative or root, insert to the
         * member of a root module. Then:
         *  - semantic3 pass will get called on the instance members.
         *  - codegen pass will get a selection chance to do/skip it.
         */

        struct N
        {
            static Dsymbol *getStrictEnclosing(TemplateInstance *ti)
            {
                do
                {
                    if (ti->enclosing)
                        return ti->enclosing;
                    ti = ti->tempdecl->isInstantiated();
                }
                while (ti);
                return NULL;
            }
        };
        Dsymbol *enc = N::getStrictEnclosing(this);

        // insert target is made stable by using the module
        // where tempdecl is declared.
        mi = (enc ? enc : tempdecl)->getModule();
        if (!mi->isRoot())
            mi = mi->importedFrom;
        assert(mi->isRoot());
    }
    else
    {
        /* If the instantiated module is non-root, insert to the member of the
         * non-root module. Then:
         *  - semantic3 pass won't be called on the instance.
         *  - codegen pass won't reach to the instance.
         */
    }
    //printf("\t--> mi = %s\n", mi->toPrettyChars());

    if (memberOf == mi)     // already a member
    {
        return NULL;
    }

    Dsymbols *a = mi->members;
    a->push(this);
    memberOf = mi;
    if (mi->semanticRun >= PASSsemantic2done && mi->isRoot())
        Module::addDeferredSemantic2(this);
    if (mi->semanticRun >= PASSsemantic3done && mi->isRoot())
        Module::addDeferredSemantic3(this);
    return a;
}

/****************************************
 * This instance needs an identifier for name mangling purposes.
 * Create one by taking the template declaration name and adding
 * the type signature for it.
 */

Identifier *TemplateInstance::genIdent(Objects *args)
{
    TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
    assert(tempdecl);

    //printf("TemplateInstance::genIdent('%s')\n", tempdecl->ident->toChars());
    OutBuffer buf;
    const char *id = tempdecl->ident->toChars();
    if (!members)
    {
        // Use "__U" for the symbols declared inside template constraint.
        buf.printf("__U%llu%s", (ulonglong)strlen(id), id);
    }
    else
        buf.printf("__T%llu%s", (ulonglong)strlen(id), id);
    size_t nparams = tempdecl->parameters->dim - (tempdecl->isVariadic() ? 1 : 0);
    for (size_t i = 0; i < args->dim; i++)
    {
        RootObject *o = (*args)[i];
        Type *ta = isType(o);
        Expression *ea = isExpression(o);
        Dsymbol *sa = isDsymbol(o);
        Tuple *va = isTuple(o);
        //printf("\to [%d] %p ta %p ea %p sa %p va %p\n", i, o, ta, ea, sa, va);
        if (i < nparams && (*tempdecl->parameters)[i]->specialization())
            buf.writeByte('H');     // Bugzilla 6574
        if (ta)
        {
            buf.writeByte('T');
            if (ta->deco)
                buf.writestring(ta->deco);
            else
            {
                assert(global.errors);
            }
        }
        else if (ea)
        {
            // Don't interpret it yet, it might actually be an alias template parameter.
            // Only constfold manifest constants, not const/immutable lvalues, see https://issues.dlang.org/show_bug.cgi?id=17339.
            const bool keepLvalue = true;
            ea = ea->optimize(WANTvalue, keepLvalue);
            if (ea->op == TOKvar)
            {
                sa = ((VarExp *)ea)->var;
                ea = NULL;
                goto Lsa;
            }
            if (ea->op == TOKthis)
            {
                sa = ((ThisExp *)ea)->var;
                ea = NULL;
                goto Lsa;
            }
            if (ea->op == TOKfunction)
            {
                if (((FuncExp *)ea)->td)
                    sa = ((FuncExp *)ea)->td;
                else
                    sa = ((FuncExp *)ea)->fd;
                ea = NULL;
                goto Lsa;
            }
            buf.writeByte('V');
            if (ea->op == TOKtuple)
            {
                ea->error("tuple is not a valid template value argument");
                continue;
            }
            // Now that we know it is not an alias, we MUST obtain a value
            unsigned olderr = global.errors;
            ea = ea->ctfeInterpret();
            if (ea->op == TOKerror || olderr != global.errors)
                continue;

            /* Use deco that matches what it would be for a function parameter
             */
            buf.writestring(ea->type->deco);
            mangleToBuffer(ea, &buf);
        }
        else if (sa)
        {
          Lsa:
            buf.writeByte('S');
            sa = sa->toAlias();
            Declaration *d = sa->isDeclaration();
            if (d && (!d->type || !d->type->deco))
            {
                error("forward reference of %s %s", d->kind(), d->toChars());
                continue;
            }

            OutBuffer bufsa;
            mangleToBuffer(sa, &bufsa);
            const char *s = bufsa.extractString();

            /* Bugzilla 3043: if the first character of s is a digit this
             * causes ambiguity issues because the digits of the two numbers are adjacent.
             * Current demanglers resolve this by trying various places to separate the
             * numbers until one gets a successful demangle.
             * Unfortunately, fixing this ambiguity will break existing binary
             * compatibility and the demanglers, so we'll leave it as is.
             */
            buf.printf("%u%s", (unsigned)strlen(s), s);
        }
        else if (va)
        {
            assert(i + 1 == args->dim);         // must be last one
            args = &va->objects;
            i = -(size_t)1;
        }
        else
            assert(0);
    }
    buf.writeByte('Z');
    id = buf.peekString();
    //printf("\tgenIdent = %s\n", id);
    return Identifier::idPool(id);
}

/*************************************
 * Lazily generate identifier for template instance.
 * This is because 75% of the ident's are never needed.
 */

Identifier *TemplateInstance::getIdent()
{
    if (!ident && inst && !errors)
        ident = genIdent(tiargs);         // need an identifier for name mangling purposes.
    return ident;
}

/****************************************************
 * Declare parameters of template instance, initialize them with the
 * template instance arguments.
 */

void TemplateInstance::declareParameters(Scope *sc)
{
    TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
    assert(tempdecl);

    //printf("TemplateInstance::declareParameters()\n");
    for (size_t i = 0; i < tdtypes.dim; i++)
    {
        TemplateParameter *tp = (*tempdecl->parameters)[i];
        //RootObject *o = (*tiargs)[i];
        RootObject *o = tdtypes[i];          // initializer for tp

        //printf("\ttdtypes[%d] = %p\n", i, o);
        tempdecl->declareParameter(sc, tp, o);
    }
}

void TemplateInstance::semantic2(Scope *sc)
{
    if (semanticRun >= PASSsemantic2)
        return;
    semanticRun = PASSsemantic2;
    if (!errors && members)
    {
        TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
        assert(tempdecl);

        sc = tempdecl->_scope;
        assert(sc);
        sc = sc->push(argsym);
        sc = sc->push(this);
        sc->tinst = this;
        sc->minst = minst;

        int needGagging = (gagged && !global.gag);
        unsigned int olderrors = global.errors;
        int oldGaggedErrors = -1;       // dead-store to prevent spurious warning
        if (needGagging)
            oldGaggedErrors = global.startGagging();

        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            s->semantic2(sc);
            if (gagged && global.errors != olderrors)
                break;
        }

        if (global.errors != olderrors)
        {
            if (!errors)
            {
                if (!tempdecl->literal)
                    error(loc, "error instantiating");
                if (tinst)
                    tinst->printInstantiationTrace();
            }
            errors = true;
        }
        if (needGagging)
            global.endGagging(oldGaggedErrors);

        sc = sc->pop();
        sc->pop();
    }
}

void TemplateInstance::semantic3(Scope *sc)
{
//if (toChars()[0] == 'D') *(char*)0=0;
    if (semanticRun >= PASSsemantic3)
        return;
    semanticRun = PASSsemantic3;
    if (!errors && members)
    {
        TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
        assert(tempdecl);

        sc = tempdecl->_scope;
        sc = sc->push(argsym);
        sc = sc->push(this);
        sc->tinst = this;
        sc->minst = minst;

        int needGagging = (gagged && !global.gag);
        unsigned int olderrors = global.errors;
        int oldGaggedErrors = -1;       // dead-store to prevent spurious warning
        /* If this is a gagged instantiation, gag errors.
         * Future optimisation: If the results are actually needed, errors
         * would already be gagged, so we don't really need to run semantic
         * on the members.
         */
        if (needGagging)
            oldGaggedErrors = global.startGagging();

        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            s->semantic3(sc);
            if (gagged && global.errors != olderrors)
                break;
        }

        if (global.errors != olderrors)
        {
            if (!errors)
            {
                if (!tempdecl->literal)
                    error(loc, "error instantiating");
                if (tinst)
                    tinst->printInstantiationTrace();
            }
            errors = true;
        }
        if (needGagging)
            global.endGagging(oldGaggedErrors);

        sc = sc->pop();
        sc->pop();
    }
}

/**************************************
 * Given an error instantiating the TemplateInstance,
 * give the nested TemplateInstance instantiations that got
 * us here. Those are a list threaded into the nested scopes.
 */
void TemplateInstance::printInstantiationTrace()
{
    if (global.gag)
        return;

    const unsigned max_shown = 6;
    const char format[] = "instantiated from here: %s";

    // determine instantiation depth and number of recursive instantiations
    unsigned n_instantiations = 1;
    unsigned n_totalrecursions = 0;
    for (TemplateInstance *cur = this; cur; cur = cur->tinst)
    {
        ++n_instantiations;
        // If two instantiations use the same declaration, they are recursive.
        // (this works even if they are instantiated from different places in the
        // same template).
        // In principle, we could also check for multiple-template recursion, but it's
        // probably not worthwhile.
        if (cur->tinst && cur->tempdecl && cur->tinst->tempdecl
            && cur->tempdecl->loc.equals(cur->tinst->tempdecl->loc))
            ++n_totalrecursions;
    }

    // show full trace only if it's short or verbose is on
    if (n_instantiations <= max_shown || global.params.verbose)
    {
        for (TemplateInstance *cur = this; cur; cur = cur->tinst)
        {
            cur->errors = true;
            errorSupplemental(cur->loc, format, cur->toChars());
        }
    }
    else if (n_instantiations - n_totalrecursions <= max_shown)
    {
        // By collapsing recursive instantiations into a single line,
        // we can stay under the limit.
        int recursionDepth=0;
        for (TemplateInstance *cur = this; cur; cur = cur->tinst)
        {
            cur->errors = true;
            if (cur->tinst && cur->tempdecl && cur->tinst->tempdecl
                    && cur->tempdecl->loc.equals(cur->tinst->tempdecl->loc))
            {
                ++recursionDepth;
            }
            else
            {
                if (recursionDepth)
                    errorSupplemental(cur->loc, "%d recursive instantiations from here: %s", recursionDepth+2, cur->toChars());
                else
                    errorSupplemental(cur->loc, format, cur->toChars());
                recursionDepth = 0;
            }
        }
    }
    else
    {
        // Even after collapsing the recursions, the depth is too deep.
        // Just display the first few and last few instantiations.
        unsigned i = 0;
        for (TemplateInstance *cur = this; cur; cur = cur->tinst)
        {
            cur->errors = true;

            if (i == max_shown / 2)
                errorSupplemental(cur->loc, "... (%d instantiations, -v to show) ...", n_instantiations - max_shown);

            if (i < max_shown / 2 ||
                i >= n_instantiations - max_shown + max_shown / 2)
                errorSupplemental(cur->loc, format, cur->toChars());
            ++i;
        }
    }
}

Dsymbol *TemplateInstance::toAlias()
{
    if (!inst)
    {
        // Maybe we can resolve it
        if (_scope)
        {
            semantic(_scope);
        }
        if (!inst)
        {
            error("cannot resolve forward reference");
            errors = true;
            return this;
        }
    }

    if (inst != this)
        return inst->toAlias();

    if (aliasdecl)
    {
        return aliasdecl->toAlias();
    }

    return inst;
}

const char *TemplateInstance::kind() const
{
    return "template instance";
}

bool TemplateInstance::oneMember(Dsymbol **ps, Identifier *)
{
    *ps = NULL;
    return true;
}

const char *TemplateInstance::toChars()
{
    OutBuffer buf;
    toCBufferInstance(this, &buf);
    return buf.extractString();
}

const char *TemplateInstance::toPrettyCharsHelper()
{
    OutBuffer buf;
    toCBufferInstance(this, &buf, true);
    return buf.extractString();
}

/*************************************
 * Compare proposed template instantiation with existing template instantiation.
 * Note that this is not commutative because of the auto ref check.
 * Params:
 *  this = proposed template instantiation
 *  o = existing template instantiation
 * Returns:
 *  0 for match, 1 for no match
 */
int TemplateInstance::compare(RootObject *o)
{
    TemplateInstance *ti = (TemplateInstance *)o;

    //printf("this = %p, ti = %p\n", this, ti);
    assert(tdtypes.dim == ti->tdtypes.dim);

    // Nesting must match
    if (enclosing != ti->enclosing)
    {
        //printf("test2 enclosing %s ti->enclosing %s\n", enclosing ? enclosing->toChars() : "", ti->enclosing ? ti->enclosing->toChars() : "");
        goto Lnotequals;
    }
    //printf("parent = %s, ti->parent = %s\n", parent->toPrettyChars(), ti->parent->toPrettyChars());

    if (!arrayObjectMatch(&tdtypes, &ti->tdtypes))
        goto Lnotequals;

    /* Template functions may have different instantiations based on
     * "auto ref" parameters.
     */
    if (FuncDeclaration *fd = ti->toAlias()->isFuncDeclaration())
    {
        if (!fd->errors)
        {
            Parameters *fparameters = fd->getParameters(NULL);
            size_t nfparams = Parameter::dim(fparameters);   // Num function parameters
            for (size_t j = 0; j < nfparams; j++)
            {
                Parameter *fparam = Parameter::getNth(fparameters, j);
                if (fparam->storageClass & STCautoref)       // if "auto ref"
                {
                    if (!fargs)
                        goto Lnotequals;
                    if (fargs->dim <= j)
                        break;
                    Expression *farg = (*fargs)[j];
                    if (farg->isLvalue())
                    {
                        if (!(fparam->storageClass & STCref))
                            goto Lnotequals;                // auto ref's don't match
                    }
                    else
                    {
                        if (fparam->storageClass & STCref)
                            goto Lnotequals;                // auto ref's don't match
                    }
                }
            }
        }
    }
    return 0;

  Lnotequals:
    return 1;
}

hash_t TemplateInstance::toHash()
{
    if (!hash)
    {
        hash = (size_t)(void *)enclosing;
        hash += arrayObjectHash(&tdtypes);
        hash += hash == 0;
    }
    return hash;
}

/**************************************
 * IsExpression can evaluate the specified type speculatively, and even if
 * it instantiates any symbols, they are normally unnecessary for the
 * final executable.
 * However, if those symbols leak to the actual code, compiler should remark
 * them as non-speculative to generate their code and link to the final executable.
 */
void unSpeculative(Scope *sc, RootObject *o)
{
    if (!o)
        return;

    if (Tuple *tup = isTuple(o))
    {
        for (size_t i = 0; i < tup->objects.dim; i++)
        {
            unSpeculative(sc, tup->objects[i]);
        }
        return;
    }

    Dsymbol *s = getDsymbol(o);
    if (!s)
        return;

    if (Declaration *d = s->isDeclaration())
    {
        if (VarDeclaration *vd = d->isVarDeclaration())
            o = vd->type;
        else if (AliasDeclaration *ad = d->isAliasDeclaration())
        {
            o = ad->getType();
            if (!o)
                o = ad->toAlias();
        }
        else
            o = d->toAlias();

        s = getDsymbol(o);
        if (!s)
            return;
    }

    if (TemplateInstance *ti = s->isTemplateInstance())
    {
        // If the instance is already non-speculative,
        // or it is leaked to the speculative scope.
        if (ti->minst != NULL || sc->minst == NULL)
            return;

        // Remark as non-speculative instance.
        ti->minst = sc->minst;
        if (!ti->tinst)
            ti->tinst = sc->tinst;

        unSpeculative(sc, ti->tempdecl);
    }

    if (TemplateInstance *ti = s->isInstantiated())
        unSpeculative(sc, ti);
}

/***********************************************
 * Returns true if this is not instantiated in non-root module, and
 * is a part of non-speculative instantiatiation.
 *
 * Note: minst does not stabilize until semantic analysis is completed,
 * so don't call this function during semantic analysis to return precise result.
 */
bool TemplateInstance::needsCodegen()
{
    // Now -allInst is just for the backward compatibility.
    if (global.params.allInst)
    {
        //printf("%s minst = %s, enclosing (%s)->isNonRoot = %d\n",
        //    toPrettyChars(), minst ? minst->toChars() : NULL,
        //    enclosing ? enclosing->toPrettyChars() : NULL, enclosing && enclosing->inNonRoot());
        if (enclosing)
        {
            // Bugzilla 14588: If the captured context is not a function
            // (e.g. class), the instance layout determination is guaranteed,
            // because the semantic/semantic2 pass will be executed
            // even for non-root instances.
            if (!enclosing->isFuncDeclaration())
                return true;

            // Bugzilla 14834: If the captured context is a function,
            // this excessive instantiation may cause ODR violation, because
            // -allInst and others doesn't guarantee the semantic3 execution
            // for that function.

            // If the enclosing is also an instantiated function,
            // we have to rely on the ancestor's needsCodegen() result.
            if (TemplateInstance *ti = enclosing->isInstantiated())
                return ti->needsCodegen();

            // Bugzilla 13415: If and only if the enclosing scope needs codegen,
            // this nested templates would also need code generation.
            return !enclosing->inNonRoot();
        }
        return true;
    }

    if (!minst)
    {
        // If this is a speculative instantiation,
        // 1. do codegen if ancestors really needs codegen.
        // 2. become non-speculative if siblings are not speculative

        TemplateInstance *tnext = this->tnext;
        TemplateInstance *tinst = this->tinst;
        // At first, disconnect chain first to prevent infinite recursion.
        this->tnext = NULL;
        this->tinst = NULL;

        // Determine necessity of tinst before tnext.
        if (tinst && tinst->needsCodegen())
        {
            minst = tinst->minst;   // cache result
            assert(minst);
            assert(minst->isRoot() || minst->rootImports());
            return true;
        }
        if (tnext && (tnext->needsCodegen() || tnext->minst))
        {
            minst = tnext->minst;   // cache result
            assert(minst);
            return minst->isRoot() || minst->rootImports();
        }

        // Elide codegen because this is really speculative.
        return false;
    }

    /* Even when this is reached to the codegen pass,
     * a non-root nested template should not generate code,
     * due to avoid ODR violation.
     */
    if (enclosing && enclosing->inNonRoot())
    {
        if (tinst)
        {
            bool r = tinst->needsCodegen();
            minst = tinst->minst; // cache result
            return r;
        }
        if (tnext)
        {
            bool r = tnext->needsCodegen();
            minst = tnext->minst; // cache result
            return r;
        }
        return false;
    }

    /* The issue is that if the importee is compiled with a different -debug
     * setting than the importer, the importer may believe it exists
     * in the compiled importee when it does not, when the instantiation
     * is behind a conditional debug declaration.
     */
    // workaround for Bugzilla 11239
    if (global.params.useUnitTests ||
        global.params.debuglevel)
    {
        // Prefer instantiations from root modules, to maximize link-ability.
        if (minst->isRoot())
            return true;

        TemplateInstance *tnext = this->tnext;
        TemplateInstance *tinst = this->tinst;
        this->tnext = NULL;
        this->tinst = NULL;

        if (tinst && tinst->needsCodegen())
        {
            minst = tinst->minst;   // cache result
            assert(minst);
            assert(minst->isRoot() || minst->rootImports());
            return true;
        }
        if (tnext && tnext->needsCodegen())
        {
            minst = tnext->minst;   // cache result
            assert(minst);
            assert(minst->isRoot() || minst->rootImports());
            return true;
        }

        // Bugzilla 2500 case
        if (minst->rootImports())
            return true;

        // Elide codegen because this is not included in root instances.
        return false;
    }
    else
    {
        // Prefer instantiations from non-root module, to minimize object code size.

        /* If a TemplateInstance is ever instantiated by non-root modules,
         * we do not have to generate code for it,
         * because it will be generated when the non-root module is compiled.
         *
         * But, if the non-root 'minst' imports any root modules, it might still need codegen.
         *
         * The problem is if A imports B, and B imports A, and both A
         * and B instantiate the same template, does the compilation of A
         * or the compilation of B do the actual instantiation?
         *
         * See Bugzilla 2500.
         */
        if (!minst->isRoot() && !minst->rootImports())
            return false;

        TemplateInstance *tnext = this->tnext;
        this->tnext = NULL;

        if (tnext && !tnext->needsCodegen() && tnext->minst)
        {
            minst = tnext->minst;   // cache result
            assert(!minst->isRoot());
            return false;
        }

        // Do codegen because this is not included in non-root instances.
        return true;
    }
}

/* ======================== TemplateMixin ================================ */

TemplateMixin::TemplateMixin(Loc loc, Identifier *ident, TypeQualified *tqual, Objects *tiargs)
        : TemplateInstance(loc, tqual->idents.dim ? (Identifier *)tqual->idents[tqual->idents.dim - 1]
                                                  : ((TypeIdentifier *)tqual)->ident)
{
    //printf("TemplateMixin(ident = '%s')\n", ident ? ident->toChars() : "");
    this->ident = ident;
    this->tqual = tqual;
    this->tiargs = tiargs ? tiargs : new Objects();
}

Dsymbol *TemplateMixin::syntaxCopy(Dsymbol *)
{
    TemplateMixin *tm = new TemplateMixin(loc, ident,
                (TypeQualified *)tqual->syntaxCopy(), tiargs);
    return TemplateInstance::syntaxCopy(tm);
}

bool TemplateMixin::findTempDecl(Scope *sc)
{
    // Follow qualifications to find the TemplateDeclaration
    if (!tempdecl)
    {
        Expression *e;
        Type *t;
        Dsymbol *s;
        tqual->resolve(loc, sc, &e, &t, &s);
        if (!s)
        {
            error("is not defined");
            return false;
        }
        s = s->toAlias();
        tempdecl = s->isTemplateDeclaration();
        OverloadSet *os = s->isOverloadSet();

        /* If an OverloadSet, look for a unique member that is a template declaration
         */
        if (os)
        {
            Dsymbol *ds = NULL;
            for (size_t i = 0; i < os->a.dim; i++)
            {
                Dsymbol *s2 = os->a[i]->isTemplateDeclaration();
                if (s2)
                {
                    if (ds)
                    {
                        tempdecl = os;
                        break;
                    }
                    ds = s2;
                }
            }
        }
        if (!tempdecl)
        {
            error("%s isn't a template", s->toChars());
            return false;
        }
    }
    assert(tempdecl);

  struct ParamFwdResTm
  {
    static int fp(void *param, Dsymbol *s)
    {
        TemplateDeclaration *td = s->isTemplateDeclaration();
        if (!td)
            return 0;

        TemplateMixin *tm = (TemplateMixin *)param;
        if (td->semanticRun == PASSinit)
        {
            if (td->_scope)
                td->semantic(td->_scope);
            else
            {
                tm->semanticRun = PASSinit;
                return 1;
            }
        }
        return 0;
    }
  };
    // Look for forward references
    OverloadSet *tovers = tempdecl->isOverloadSet();
    size_t overs_dim = tovers ? tovers->a.dim : 1;
    for (size_t oi = 0; oi < overs_dim; oi++)
    {
        if (overloadApply(tovers ? tovers->a[oi] : tempdecl, (void *)this, &ParamFwdResTm::fp))
            return false;
    }
    return true;
}

void TemplateMixin::semantic(Scope *sc)
{
    if (semanticRun != PASSinit)
    {
        // When a class/struct contains mixin members, and is done over
        // because of forward references, never reach here so semanticRun
        // has been reset to PASSinit.
        return;
    }
    semanticRun = PASSsemantic;

    Scope *scx = NULL;
    if (_scope)
    {
        sc = _scope;
        scx = _scope;            // save so we don't make redundant copies
        _scope = NULL;
    }

    /* Run semantic on each argument, place results in tiargs[],
     * then find best match template with tiargs
     */
    if (!findTempDecl(sc) ||
        !semanticTiargs(sc) ||
        !findBestMatch(sc, NULL))
    {
        if (semanticRun == PASSinit)    // forward reference had occured
        {
            //printf("forward reference - deferring\n");
            _scope = scx ? scx : sc->copy();
            _scope->setNoFree();
            _scope->_module->addDeferredSemantic(this);
            return;
        }

        inst = this;
        errors = true;
        return;         // error recovery
    }
    TemplateDeclaration *tempdecl = this->tempdecl->isTemplateDeclaration();
    assert(tempdecl);

    if (!ident)
    {
        /* Assign scope local unique identifier, as same as lambdas.
         */
        const char *s = "__mixin";

        DsymbolTable *symtab;
        if (FuncDeclaration *func = sc->parent->isFuncDeclaration())
        {
            symtab = func->localsymtab;
            if (symtab)
            {
                // Inside template constraint, symtab is not set yet.
                goto L1;
            }
        }
        else
        {
            symtab = sc->parent->isScopeDsymbol()->symtab;
        L1:
            assert(symtab);
            int num = (int)dmd_aaLen(symtab->tab) + 1;
            ident = Identifier::generateId(s, num);
            symtab->insert(this);
        }
    }

    inst = this;
    parent = sc->parent;

    /* Detect recursive mixin instantiations.
     */
    for (Dsymbol *s = parent; s; s = s->parent)
    {
        //printf("\ts = '%s'\n", s->toChars());
        TemplateMixin *tm = s->isTemplateMixin();
        if (!tm || tempdecl != tm->tempdecl)
            continue;

        /* Different argument list lengths happen with variadic args
         */
        if (tiargs->dim != tm->tiargs->dim)
            continue;

        for (size_t i = 0; i < tiargs->dim; i++)
        {
            RootObject *o = (*tiargs)[i];
            Type *ta = isType(o);
            Expression *ea = isExpression(o);
            Dsymbol *sa = isDsymbol(o);
            RootObject *tmo = (*tm->tiargs)[i];
            if (ta)
            {
                Type *tmta = isType(tmo);
                if (!tmta)
                    goto Lcontinue;
                if (!ta->equals(tmta))
                    goto Lcontinue;
            }
            else if (ea)
            {
                Expression *tme = isExpression(tmo);
                if (!tme || !ea->equals(tme))
                    goto Lcontinue;
            }
            else if (sa)
            {
                Dsymbol *tmsa = isDsymbol(tmo);
                if (sa != tmsa)
                    goto Lcontinue;
            }
            else
                assert(0);
        }
        error("recursive mixin instantiation");
        return;

    Lcontinue:
        continue;
    }

    // Copy the syntax trees from the TemplateDeclaration
    members = Dsymbol::arraySyntaxCopy(tempdecl->members);
    if (!members)
        return;

    symtab = new DsymbolTable();

    for (Scope *sce = sc; 1; sce = sce->enclosing)
    {
        ScopeDsymbol *sds = (ScopeDsymbol *)sce->scopesym;
        if (sds)
        {
            sds->importScope(this, Prot(PROTpublic));
            break;
        }
    }

    Scope *scy = sc->push(this);
    scy->parent = this;

    argsym = new ScopeDsymbol();
    argsym->parent = scy->parent;
    Scope *argscope = scy->push(argsym);

    unsigned errorsave = global.errors;

    // Declare each template parameter as an alias for the argument type
    declareParameters(argscope);

    // Add members to enclosing scope, as well as this scope
    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->addMember(argscope, this);
        //printf("sc->parent = %p, sc->scopesym = %p\n", sc->parent, sc->scopesym);
        //printf("s->parent = %s\n", s->parent->toChars());
    }

    // Do semantic() analysis on template instance members
    Scope *sc2 = argscope->push(this);
    //size_t deferred_dim = Module::deferred.dim;

    static int nest;
    //printf("%d\n", nest);
    if (++nest > 500)
    {
        global.gag = 0;                 // ensure error message gets printed
        error("recursive expansion");
        fatal();
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->setScope(sc2);
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->importAll(sc2);
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->semantic(sc2);
    }

    nest--;

    /* In DeclDefs scope, TemplateMixin does not have to handle deferred symbols.
     * Because the members would already call Module::addDeferredSemantic() for themselves.
     * See Struct, Class, Interface, and EnumDeclaration::semantic().
     */
    //if (!sc->func && Module::deferred.dim > deferred_dim) {}

    AggregateDeclaration *ad = toParent()->isAggregateDeclaration();
    if (sc->func && !ad)
    {
        semantic2(sc2);
        semantic3(sc2);
    }

    // Give additional context info if error occurred during instantiation
    if (global.errors != errorsave)
    {
        error("error instantiating");
        errors = true;
    }

    sc2->pop();
    argscope->pop();
    scy->pop();
}

void TemplateMixin::semantic2(Scope *sc)
{
    if (semanticRun >= PASSsemantic2)
        return;
    semanticRun = PASSsemantic2;
    if (members)
    {
        assert(sc);
        sc = sc->push(argsym);
        sc = sc->push(this);
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            s->semantic2(sc);
        }
        sc = sc->pop();
        sc->pop();
    }
}

void TemplateMixin::semantic3(Scope *sc)
{
    if (semanticRun >= PASSsemantic3)
        return;
    semanticRun = PASSsemantic3;
    if (members)
    {
        sc = sc->push(argsym);
        sc = sc->push(this);
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            s->semantic3(sc);
        }
        sc = sc->pop();
        sc->pop();
    }
}

const char *TemplateMixin::kind() const
{
    return "mixin";
}

bool TemplateMixin::oneMember(Dsymbol **ps, Identifier *ident)
{
    return Dsymbol::oneMember(ps, ident);
}

int TemplateMixin::apply(Dsymbol_apply_ft_t fp, void *param)
{
    if (_scope) // if fwd reference
        semantic(NULL); // try to resolve it
    if (members)
    {
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            if (s)
            {
                if (s->apply(fp, param))
                    return 1;
            }
        }
    }
    return 0;
}

bool TemplateMixin::hasPointers()
{
    //printf("TemplateMixin::hasPointers() %s\n", toChars());

    if (members)
    {
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf(" s = %s %s\n", s->kind(), s->toChars());
            if (s->hasPointers())
            {
                return true;
            }
        }
    }
    return false;
}

void TemplateMixin::setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion)
{
    //printf("TemplateMixin::setFieldOffset() %s\n", toChars());
    if (_scope)                  // if fwd reference
        semantic(NULL);         // try to resolve it
    if (members)
    {
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf("\t%s\n", s->toChars());
            s->setFieldOffset(ad, poffset, isunion);
        }
    }
}

const char *TemplateMixin::toChars()
{
    OutBuffer buf;
    toCBufferInstance(this, &buf);
    return buf.extractString();
}
