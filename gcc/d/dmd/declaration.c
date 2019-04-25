
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/declaration.c
 */

#include "root/dsystem.h"
#include "root/checkedint.h"

#include "errors.h"
#include "init.h"
#include "declaration.h"
#include "attrib.h"
#include "mtype.h"
#include "template.h"
#include "scope.h"
#include "aggregate.h"
#include "module.h"
#include "import.h"
#include "id.h"
#include "expression.h"
#include "statement.h"
#include "ctfe.h"
#include "target.h"
#include "hdrgen.h"

bool checkNestedRef(Dsymbol *s, Dsymbol *p);
VarDeclaration *copyToTemp(StorageClass stc, const char *name, Expression *e);
Expression *semantic(Expression *e, Scope *sc);
Initializer *inferType(Initializer *init, Scope *sc);
Initializer *semantic(Initializer *init, Scope *sc, Type *t, NeedInterpret needInterpret);

/************************************
 * Check to see the aggregate type is nested and its context pointer is
 * accessible from the current scope.
 * Returns true if error occurs.
 */
bool checkFrameAccess(Loc loc, Scope *sc, AggregateDeclaration *ad, size_t iStart = 0)
{
    Dsymbol *sparent = ad->toParent2();
    Dsymbol *s = sc->func;
    if (ad->isNested() && s)
    {
        //printf("ad = %p %s [%s], parent:%p\n", ad, ad->toChars(), ad->loc.toChars(), ad->parent);
        //printf("sparent = %p %s [%s], parent: %s\n", sparent, sparent->toChars(), sparent->loc.toChars(), sparent->parent->toChars());
        if (checkNestedRef(s, sparent))
        {
            error(loc, "cannot access frame pointer of %s", ad->toPrettyChars());
            return true;
        }
    }

    bool result = false;
    for (size_t i = iStart; i < ad->fields.dim; i++)
    {
        VarDeclaration *vd = ad->fields[i];
        Type *tb = vd->type->baseElemOf();
        if (tb->ty == Tstruct)
        {
            result |= checkFrameAccess(loc, sc, ((TypeStruct *)tb)->sym);
        }
    }
    return result;
}

/********************************* Declaration ****************************/

Declaration::Declaration(Identifier *id)
    : Dsymbol(id)
{
    type = NULL;
    originalType = NULL;
    storage_class = STCundefined;
    protection = Prot(PROTundefined);
    linkage = LINKdefault;
    inuse = 0;
    mangleOverride = NULL;
}

void Declaration::semantic(Scope *)
{
}

const char *Declaration::kind() const
{
    return "declaration";
}

d_uns64 Declaration::size(Loc)
{
    assert(type);
    return type->size();
}

bool Declaration::isDelete()
{
    return false;
}

bool Declaration::isDataseg()
{
    return false;
}

bool Declaration::isThreadlocal()
{
    return false;
}

bool Declaration::isCodeseg() const
{
    return false;
}

Prot Declaration::prot()
{
    return protection;
}

/*************************************
 * Check to see if declaration can be modified in this context (sc).
 * Issue error if not.
 */

int Declaration::checkModify(Loc loc, Scope *sc, Type *, Expression *e1, int flag)
{
    VarDeclaration *v = isVarDeclaration();
    if (v && v->canassign)
        return 2;

    if (isParameter() || isResult())
    {
        for (Scope *scx = sc; scx; scx = scx->enclosing)
        {
            if (scx->func == parent && (scx->flags & SCOPEcontract))
            {
                const char *s = isParameter() && parent->ident != Id::ensure ? "parameter" : "result";
                if (!flag) error(loc, "cannot modify %s '%s' in contract", s, toChars());
                return 2;   // do not report type related errors
            }
        }
    }

    if (v && (isCtorinit() || isField()))
    {
        // It's only modifiable if inside the right constructor
        if ((storage_class & (STCforeach | STCref)) == (STCforeach | STCref))
            return 2;
        return modifyFieldVar(loc, sc, v, e1) ? 2 : 1;
    }
    return 1;
}

Dsymbol *Declaration::search(const Loc &loc, Identifier *ident, int flags)
{
    Dsymbol *s = Dsymbol::search(loc, ident, flags);
    if (!s && type)
    {
        s = type->toDsymbol(_scope);
        if (s)
            s = s->search(loc, ident, flags);
    }
    return s;
}


/********************************* TupleDeclaration ****************************/

TupleDeclaration::TupleDeclaration(Loc loc, Identifier *id, Objects *objects)
    : Declaration(id)
{
    this->loc = loc;
    this->type = NULL;
    this->objects = objects;
    this->isexp = false;
    this->tupletype = NULL;
}

Dsymbol *TupleDeclaration::syntaxCopy(Dsymbol *)
{
    assert(0);
    return NULL;
}

const char *TupleDeclaration::kind() const
{
    return "tuple";
}

Type *TupleDeclaration::getType()
{
    /* If this tuple represents a type, return that type
     */

    //printf("TupleDeclaration::getType() %s\n", toChars());
    if (isexp)
        return NULL;
    if (!tupletype)
    {
        /* It's only a type tuple if all the Object's are types
         */
        for (size_t i = 0; i < objects->dim; i++)
        {
            RootObject *o = (*objects)[i];
            if (o->dyncast() != DYNCAST_TYPE)
            {
                //printf("\tnot[%d], %p, %d\n", i, o, o->dyncast());
                return NULL;
            }
        }

        /* We know it's a type tuple, so build the TypeTuple
         */
        Types *types = (Types *)objects;
        Parameters *args = new Parameters();
        args->setDim(objects->dim);
        OutBuffer buf;
        int hasdeco = 1;
        for (size_t i = 0; i < types->dim; i++)
        {
            Type *t = (*types)[i];
            //printf("type = %s\n", t->toChars());
            Parameter *arg = new Parameter(0, t, NULL, NULL);
            (*args)[i] = arg;
            if (!t->deco)
                hasdeco = 0;
        }

        tupletype = new TypeTuple(args);
        if (hasdeco)
            return tupletype->semantic(Loc(), NULL);
    }

    return tupletype;
}

Dsymbol *TupleDeclaration::toAlias2()
{
    //printf("TupleDeclaration::toAlias2() '%s' objects = %s\n", toChars(), objects->toChars());

    for (size_t i = 0; i < objects->dim; i++)
    {
        RootObject *o = (*objects)[i];
        if (Dsymbol *s = isDsymbol(o))
        {
            s = s->toAlias2();
            (*objects)[i] = s;
        }
    }
    return this;
}

bool TupleDeclaration::needThis()
{
    //printf("TupleDeclaration::needThis(%s)\n", toChars());
    for (size_t i = 0; i < objects->dim; i++)
    {
        RootObject *o = (*objects)[i];
        if (o->dyncast() == DYNCAST_EXPRESSION)
        {
            Expression *e = (Expression *)o;
            if (e->op == TOKdsymbol)
            {
                DsymbolExp *ve = (DsymbolExp *)e;
                Declaration *d = ve->s->isDeclaration();
                if (d && d->needThis())
                {
                    return true;
                }
            }
        }
    }
    return false;
}

/********************************* AliasDeclaration ****************************/

AliasDeclaration::AliasDeclaration(Loc loc, Identifier *id, Type *type)
    : Declaration(id)
{
    //printf("AliasDeclaration(id = '%s', type = %p)\n", id->toChars(), type);
    //printf("type = '%s'\n", type->toChars());
    this->loc = loc;
    this->type = type;
    this->aliassym = NULL;
    this->_import = NULL;
    this->overnext = NULL;
    assert(type);
}

AliasDeclaration::AliasDeclaration(Loc loc, Identifier *id, Dsymbol *s)
    : Declaration(id)
{
    //printf("AliasDeclaration(id = '%s', s = %p)\n", id->toChars(), s);
    assert(s != this);
    this->loc = loc;
    this->type = NULL;
    this->aliassym = s;
    this->_import = NULL;
    this->overnext = NULL;
    assert(s);
}

AliasDeclaration *AliasDeclaration::create(Loc loc, Identifier *id, Type *type)
{
    return new AliasDeclaration(loc, id, type);
}

Dsymbol *AliasDeclaration::syntaxCopy(Dsymbol *s)
{
    //printf("AliasDeclaration::syntaxCopy()\n");
    assert(!s);
    AliasDeclaration *sa =
        type ? new AliasDeclaration(loc, ident, type->syntaxCopy())
             : new AliasDeclaration(loc, ident, aliassym->syntaxCopy(NULL));
    sa->storage_class = storage_class;
    return sa;
}

void AliasDeclaration::semantic(Scope *sc)
{
    if (semanticRun >= PASSsemanticdone)
        return;
    assert(semanticRun <= PASSsemantic);

    storage_class |= sc->stc & STCdeprecated;
    protection = sc->protection;
    userAttribDecl = sc->userAttribDecl;

    if (!sc->func && inNonRoot())
        return;

    aliasSemantic(sc);
}

void AliasDeclaration::aliasSemantic(Scope *sc)
{
    //printf("AliasDeclaration::semantic() %s\n", toChars());
    if (aliassym)
    {
        FuncDeclaration *fd = aliassym->isFuncLiteralDeclaration();
        TemplateDeclaration *td = aliassym->isTemplateDeclaration();
        if (fd || (td && td->literal))
        {
            if (fd && fd->semanticRun >= PASSsemanticdone)
                return;

            Expression *e = new FuncExp(loc, aliassym);
            e = ::semantic(e, sc);
            if (e->op == TOKfunction)
            {
                FuncExp *fe = (FuncExp *)e;
                aliassym = fe->td ? (Dsymbol *)fe->td : fe->fd;
            }
            else
            {
                aliassym = NULL;
                type = Type::terror;
            }
            return;
        }

        if (aliassym->isTemplateInstance())
            aliassym->semantic(sc);
        return;
    }
    inuse = 1;

    // Given:
    //  alias foo.bar.abc def;
    // it is not knowable from the syntax whether this is an alias
    // for a type or an alias for a symbol. It is up to the semantic()
    // pass to distinguish.
    // If it is a type, then type is set and getType() will return that
    // type. If it is a symbol, then aliassym is set and type is NULL -
    // toAlias() will return aliasssym.

    unsigned int errors = global.errors;
    Type *oldtype = type;

    // Ungag errors when not instantiated DeclDefs scope alias
    Ungag ungag(global.gag);
    //printf("%s parent = %s, gag = %d, instantiated = %d\n", toChars(), parent, global.gag, isInstantiated());
    if (parent && global.gag && !isInstantiated() && !toParent2()->isFuncDeclaration())
    {
        //printf("%s type = %s\n", toPrettyChars(), type->toChars());
        global.gag = 0;
    }

    /* This section is needed because Type::resolve() will:
     *   const x = 3;
     *   alias y = x;
     * try to convert identifier x to 3.
     */
    Dsymbol *s = type->toDsymbol(sc);
    if (errors != global.errors)
    {
        s = NULL;
        type = Type::terror;
    }
    if (s && s == this)
    {
        error("cannot resolve");
        s = NULL;
        type = Type::terror;
    }
    if (!s || !s->isEnumMember())
    {
        Type *t;
        Expression *e;
        Scope *sc2 = sc;
        if (storage_class & (STCref | STCnothrow | STCnogc | STCpure | STCdisable))
        {
            // For 'ref' to be attached to function types, and picked
            // up by Type::resolve(), it has to go into sc.
            sc2 = sc->push();
            sc2->stc |= storage_class & (STCref | STCnothrow | STCnogc | STCpure | STCshared | STCdisable);
        }
        type = type->addSTC(storage_class);
        type->resolve(loc, sc2, &e, &t, &s);
        if (sc2 != sc)
            sc2->pop();

        if (e)  // Try to convert Expression to Dsymbol
        {
            s = getDsymbol(e);
            if (!s)
            {
                if (e->op != TOKerror)
                    error("cannot alias an expression %s", e->toChars());
                t = Type::terror;
            }
        }
        type = t;
    }
    if (s == this)
    {
        assert(global.errors);
        type = Type::terror;
        s = NULL;
    }
    if (!s) // it's a type alias
    {
        //printf("alias %s resolved to type %s\n", toChars(), type->toChars());
        type = type->semantic(loc, sc);
        aliassym = NULL;
    }
    else    // it's a symbolic alias
    {
        //printf("alias %s resolved to %s %s\n", toChars(), s->kind(), s->toChars());
        type = NULL;
        aliassym = s;
    }
    if (global.gag && errors != global.errors)
    {
        type = oldtype;
        aliassym = NULL;
    }
    inuse = 0;
    semanticRun = PASSsemanticdone;

    if (Dsymbol *sx = overnext)
    {
        overnext = NULL;

        if (!overloadInsert(sx))
            ScopeDsymbol::multiplyDefined(Loc(), sx, this);
    }
}

bool AliasDeclaration::overloadInsert(Dsymbol *s)
{
    //printf("[%s] AliasDeclaration::overloadInsert('%s') s = %s %s @ [%s]\n",
    //    loc.toChars(), toChars(), s->kind(), s->toChars(), s->loc.toChars());

    /** Aliases aren't overloadable themselves, but if their Aliasee is
     *  overloadable they are converted to an overloadable Alias (either
     *  FuncAliasDeclaration or OverDeclaration).
     *
     *  This is done by moving the Aliasee into such an overloadable alias
     *  which is then used to replace the existing Aliasee. The original
     *  Alias (_this_) remains a useless shell.
     *
     *  This is a horrible mess. It was probably done to avoid replacing
     *  existing AST nodes and references, but it needs a major
     *  simplification b/c it's too complex to maintain.
     *
     *  A simpler approach might be to merge any colliding symbols into a
     *  simple Overload class (an array) and then later have that resolve
     *  all collisions.
     */
    if (semanticRun >= PASSsemanticdone)
    {
        /* Semantic analysis is already finished, and the aliased entity
         * is not overloadable.
         */
        if (type)
            return false;

        /* When s is added in member scope by static if, mixin("code") or others,
         * aliassym is determined already. See the case in: test/compilable/test61.d
         */
        Dsymbol *sa = aliassym->toAlias();
        if (FuncDeclaration *fd = sa->isFuncDeclaration())
        {
            FuncAliasDeclaration *fa = new FuncAliasDeclaration(ident, fd);
            fa->protection = protection;
            fa->parent = parent;
            aliassym = fa;
            return aliassym->overloadInsert(s);
        }
        if (TemplateDeclaration *td = sa->isTemplateDeclaration())
        {
            OverDeclaration *od = new OverDeclaration(ident, td);
            od->protection = protection;
            od->parent = parent;
            aliassym = od;
            return aliassym->overloadInsert(s);
        }
        if (OverDeclaration *od = sa->isOverDeclaration())
        {
            if (sa->ident != ident || sa->parent != parent)
            {
                od = new OverDeclaration(ident, od);
                od->protection = protection;
                od->parent = parent;
                aliassym = od;
            }
            return od->overloadInsert(s);
        }
        if (OverloadSet *os = sa->isOverloadSet())
        {
            if (sa->ident != ident || sa->parent != parent)
            {
                os = new OverloadSet(ident, os);
                // TODO: protection is lost here b/c OverloadSets have no protection attribute
                // Might no be a practical issue, b/c the code below fails to resolve the overload anyhow.
                // ----
                // module os1;
                // import a, b;
                // private alias merged = foo; // private alias to overload set of a.foo and b.foo
                // ----
                // module os2;
                // import a, b;
                // public alias merged = bar; // public alias to overload set of a.bar and b.bar
                // ----
                // module bug;
                // import os1, os2;
                // void test() { merged(123); } // should only look at os2.merged
                //
                // os.protection = protection;
                os->parent = parent;
                aliassym = os;
            }
            os->push(s);
            return true;
        }
        return false;
    }

    /* Don't know yet what the aliased symbol is, so assume it can
     * be overloaded and check later for correctness.
     */
    if (overnext)
        return overnext->overloadInsert(s);
    if (s == this)
        return true;
    overnext = s;
    return true;
}

const char *AliasDeclaration::kind() const
{
    return "alias";
}

Type *AliasDeclaration::getType()
{
    if (type)
        return type;
    return toAlias()->getType();
}

Dsymbol *AliasDeclaration::toAlias()
{
    //printf("[%s] AliasDeclaration::toAlias('%s', this = %p, aliassym = %p, kind = '%s', inuse = %d)\n",
    //    loc.toChars(), toChars(), this, aliassym, aliassym ? aliassym->kind() : "", inuse);
    assert(this != aliassym);
    //static int count; if (++count == 10) *(char*)0=0;
    if (inuse == 1 && type && _scope)
    {
        inuse = 2;
        unsigned olderrors = global.errors;
        Dsymbol *s = type->toDsymbol(_scope);
        //printf("[%s] type = %s, s = %p, this = %p\n", loc.toChars(), type->toChars(), s, this);
        if (global.errors != olderrors)
            goto Lerr;
        if (s)
        {
            s = s->toAlias();
            if (global.errors != olderrors)
                goto Lerr;
            aliassym = s;
            inuse = 0;
        }
        else
        {
            Type *t = type->semantic(loc, _scope);
            if (t->ty == Terror)
                goto Lerr;
            if (global.errors != olderrors)
                goto Lerr;
            //printf("t = %s\n", t->toChars());
            inuse = 0;
        }
    }
    if (inuse)
    {
        error("recursive alias declaration");

    Lerr:
        // Avoid breaking "recursive alias" state during errors gagged
        if (global.gag)
            return this;

        aliassym = new AliasDeclaration(loc, ident, Type::terror);
        type = Type::terror;
        return aliassym;
    }

    if (semanticRun >= PASSsemanticdone)
    {
        // semantic is already done.

        // Do not see aliassym !is null, because of lambda aliases.

        // Do not see type.deco !is null, even so "alias T = const int;` needs
        // semantic analysis to take the storage class `const` as type qualifier.
    }
    else
    {
        if (_import && _import->_scope)
        {
            /* If this is an internal alias for selective/renamed import,
             * load the module first.
             */
            _import->semantic(NULL);
        }
        if (_scope)
        {
            aliasSemantic(_scope);
        }
    }

    inuse = 1;
    Dsymbol *s = aliassym ? aliassym->toAlias() : this;
    inuse = 0;
    return s;
}

Dsymbol *AliasDeclaration::toAlias2()
{
    if (inuse)
    {
        error("recursive alias declaration");
        return this;
    }
    inuse = 1;
    Dsymbol *s  = aliassym ? aliassym->toAlias2() : this;
    inuse = 0;
    return s;
}

bool AliasDeclaration::isOverloadable()
{
    // assume overloadable until alias is resolved
    return semanticRun < PASSsemanticdone ||
        (aliassym && aliassym->isOverloadable());
}

/****************************** OverDeclaration **************************/

OverDeclaration::OverDeclaration(Identifier *ident, Dsymbol *s, bool hasOverloads)
    : Declaration(ident)
{
    this->overnext = NULL;
    this->aliassym = s;

    this->hasOverloads = hasOverloads;
    if (hasOverloads)
    {
        if (OverDeclaration *od = aliassym->isOverDeclaration())
            this->hasOverloads = od->hasOverloads;
    }
    else
    {
        // for internal use
        assert(!aliassym->isOverDeclaration());
    }
}

const char *OverDeclaration::kind() const
{
    return "overload alias";    // todo
}

void OverDeclaration::semantic(Scope *)
{
}

bool OverDeclaration::equals(RootObject *o)
{
    if (this == o)
        return true;

    Dsymbol *s = isDsymbol(o);
    if (!s)
        return false;

    OverDeclaration *od1 = this;
    if (OverDeclaration *od2 = s->isOverDeclaration())
    {
        return od1->aliassym->equals(od2->aliassym) &&
               od1->hasOverloads == od2->hasOverloads;
    }
    if (aliassym == s)
    {
        if (hasOverloads)
            return true;
        if (FuncDeclaration *fd = s->isFuncDeclaration())
        {
            return fd->isUnique() != NULL;
        }
        if (TemplateDeclaration *td = s->isTemplateDeclaration())
        {
            return td->overnext == NULL;
        }
    }
    return false;
}

bool OverDeclaration::overloadInsert(Dsymbol *s)
{
    //printf("OverDeclaration::overloadInsert('%s') aliassym = %p, overnext = %p\n", s->toChars(), aliassym, overnext);
    if (overnext)
        return overnext->overloadInsert(s);
    if (s == this)
        return true;
    overnext = s;
    return true;
}

Dsymbol *OverDeclaration::toAlias()
{
    return this;
}

bool OverDeclaration::isOverloadable()
{
    return true;
}

Dsymbol *OverDeclaration::isUnique()
{
    if (!hasOverloads)
    {
        if (aliassym->isFuncDeclaration() ||
            aliassym->isTemplateDeclaration())
        {
            return aliassym;
        }
    }

  struct ParamUniqueSym
  {
    static int fp(void *param, Dsymbol *s)
    {
        Dsymbol **ps = (Dsymbol **)param;
        if (*ps)
        {
            *ps = NULL;
            return 1;   // ambiguous, done
        }
        else
        {
            *ps = s;
            return 0;
        }
    }
  };
    Dsymbol *result = NULL;
    overloadApply(aliassym, &result, &ParamUniqueSym::fp);
    return result;
}

/********************************* VarDeclaration ****************************/

VarDeclaration::VarDeclaration(Loc loc, Type *type, Identifier *id, Initializer *init)
    : Declaration(id)
{
    //printf("VarDeclaration('%s')\n", id->toChars());
    assert(id);
    assert(type || init);
    this->type = type;
    this->_init = init;
    this->loc = loc;
    offset = 0;
    isargptr = false;
    alignment = 0;
    ctorinit = 0;
    aliassym = NULL;
    onstack = false;
    mynew = false;
    canassign = 0;
    overlapped = false;
    overlapUnsafe = false;
    doNotInferScope = false;
    isdataseg = 0;
    lastVar = NULL;
    endlinnum = 0;
    ctfeAdrOnStack = -1;
    edtor = NULL;
    range = NULL;

    static unsigned nextSequenceNumber = 0;
    this->sequenceNumber = ++nextSequenceNumber;
}

Dsymbol *VarDeclaration::syntaxCopy(Dsymbol *s)
{
    //printf("VarDeclaration::syntaxCopy(%s)\n", toChars());
    assert(!s);
    VarDeclaration *v = new VarDeclaration(loc,
            type ? type->syntaxCopy() : NULL,
            ident,
            _init ? _init->syntaxCopy() : NULL);
    v->storage_class = storage_class;
    return v;
}


void VarDeclaration::semantic(Scope *sc)
{
//    if (semanticRun > PASSinit)
//      return;
//    semanticRun = PASSsemantic;

    if (semanticRun >= PASSsemanticdone)
        return;

    Scope *scx = NULL;
    if (_scope)
    {
        sc = _scope;
        scx = sc;
        _scope = NULL;
    }

    /* Pick up storage classes from context, but except synchronized,
     * override, abstract, and final.
     */
    storage_class |= (sc->stc & ~(STCsynchronized | STCoverride | STCabstract | STCfinal));
    if (storage_class & STCextern && _init)
        error("extern symbols cannot have initializers");

    userAttribDecl = sc->userAttribDecl;

    AggregateDeclaration *ad = isThis();
    if (ad)
        storage_class |= ad->storage_class & STC_TYPECTOR;

    /* If auto type inference, do the inference
     */
    int inferred = 0;
    if (!type)
    {
        inuse++;

        // Infering the type requires running semantic,
        // so mark the scope as ctfe if required
        bool needctfe = (storage_class & (STCmanifest | STCstatic)) != 0;
        if (needctfe) sc = sc->startCTFE();

        //printf("inferring type for %s with init %s\n", toChars(), _init->toChars());
        _init = inferType(_init, sc);
        type = initializerToExpression(_init)->type;

        if (needctfe) sc = sc->endCTFE();

        inuse--;
        inferred = 1;

        /* This is a kludge to support the existing syntax for RAII
         * declarations.
         */
        storage_class &= ~STCauto;
        originalType = type->syntaxCopy();
    }
    else
    {
        if (!originalType)
            originalType = type->syntaxCopy();

        /* Prefix function attributes of variable declaration can affect
         * its type:
         *      pure nothrow void function() fp;
         *      static assert(is(typeof(fp) == void function() pure nothrow));
         */
        Scope *sc2 = sc->push();
        sc2->stc |= (storage_class & STC_FUNCATTR);
        inuse++;
        type = type->semantic(loc, sc2);
        inuse--;
        sc2->pop();
    }
    //printf(" semantic type = %s\n", type ? type->toChars() : "null");
    if (type->ty == Terror)
        errors = true;

    type->checkDeprecated(loc, sc);
    linkage = sc->linkage;
    this->parent = sc->parent;
    //printf("this = %p, parent = %p, '%s'\n", this, parent, parent->toChars());
    protection = sc->protection;

    /* If scope's alignment is the default, use the type's alignment,
     * otherwise the scope overrrides.
     */
    alignment = sc->alignment();
    if (alignment == STRUCTALIGN_DEFAULT)
        alignment = type->alignment();          // use type's alignment

    //printf("sc->stc = %x\n", sc->stc);
    //printf("storage_class = x%x\n", storage_class);

    if (global.params.vcomplex)
        type->checkComplexTransition(loc);

    // Calculate type size + safety checks
    if (sc->func && !sc->intypeof)
    {
        if ((storage_class & STCgshared) && !isMember())
        {
            if (sc->func->setUnsafe())
                error("__gshared not allowed in safe functions; use shared");
        }
    }

    Dsymbol *parent = toParent();

    Type *tb = type->toBasetype();
    Type *tbn = tb->baseElemOf();
    if (tb->ty == Tvoid && !(storage_class & STClazy))
    {
        if (inferred)
        {
            error("type %s is inferred from initializer %s, and variables cannot be of type void",
                type->toChars(), _init->toChars());
        }
        else
            error("variables cannot be of type void");
        type = Type::terror;
        tb = type;
    }
    if (tb->ty == Tfunction)
    {
        error("cannot be declared to be a function");
        type = Type::terror;
        tb = type;
    }
    if (tb->ty == Tstruct)
    {
        TypeStruct *ts = (TypeStruct *)tb;
        if (!ts->sym->members)
        {
            error("no definition of struct %s", ts->toChars());
        }
    }
    if ((storage_class & STCauto) && !inferred)
        error("storage class 'auto' has no effect if type is not inferred, did you mean 'scope'?");

    if (tb->ty == Ttuple)
    {
        /* Instead, declare variables for each of the tuple elements
         * and add those.
         */
        TypeTuple *tt = (TypeTuple *)tb;
        size_t nelems = Parameter::dim(tt->arguments);
        Expression *ie = (_init && !_init->isVoidInitializer()) ? initializerToExpression(_init) : NULL;
        if (ie)
            ie = ::semantic(ie, sc);

        if (nelems > 0 && ie)
        {
            Expressions *iexps = new Expressions();
            iexps->push(ie);

            Expressions *exps = new Expressions();

            for (size_t pos = 0; pos < iexps->dim; pos++)
            {
            Lexpand1:
                Expression *e = (*iexps)[pos];
                Parameter *arg = Parameter::getNth(tt->arguments, pos);
                arg->type = arg->type->semantic(loc, sc);
                //printf("[%d] iexps->dim = %d, ", pos, iexps->dim);
                //printf("e = (%s %s, %s), ", Token::tochars[e->op], e->toChars(), e->type->toChars());
                //printf("arg = (%s, %s)\n", arg->toChars(), arg->type->toChars());

                if (e != ie)
                {
                if (iexps->dim > nelems)
                    goto Lnomatch;
                if (e->type->implicitConvTo(arg->type))
                    continue;
                }

                if (e->op == TOKtuple)
                {
                    TupleExp *te = (TupleExp *)e;
                    if (iexps->dim - 1 + te->exps->dim > nelems)
                        goto Lnomatch;

                    iexps->remove(pos);
                    iexps->insert(pos, te->exps);
                    (*iexps)[pos] = Expression::combine(te->e0, (*iexps)[pos]);
                    goto Lexpand1;
                }
                else if (isAliasThisTuple(e))
                {
                    VarDeclaration *v = copyToTemp(0, "__tup", e);
                    VarExp *ve = new VarExp(loc, v);
                    ve->type = e->type;

                    exps->setDim(1);
                    (*exps)[0] = ve;
                    expandAliasThisTuples(exps, 0);

                    for (size_t u = 0; u < exps->dim ; u++)
                    {
                    Lexpand2:
                        Expression *ee = (*exps)[u];
                        arg = Parameter::getNth(tt->arguments, pos + u);
                        arg->type = arg->type->semantic(loc, sc);
                        //printf("[%d+%d] exps->dim = %d, ", pos, u, exps->dim);
                        //printf("ee = (%s %s, %s), ", Token::tochars[ee->op], ee->toChars(), ee->type->toChars());
                        //printf("arg = (%s, %s)\n", arg->toChars(), arg->type->toChars());

                        size_t iexps_dim = iexps->dim - 1 + exps->dim;
                        if (iexps_dim > nelems)
                            goto Lnomatch;
                        if (ee->type->implicitConvTo(arg->type))
                            continue;

                        if (expandAliasThisTuples(exps, u) != -1)
                            goto Lexpand2;
                    }

                    if ((*exps)[0] != ve)
                    {
                        Expression *e0 = (*exps)[0];
                        (*exps)[0] = new CommaExp(loc, new DeclarationExp(loc, v), e0);
                        (*exps)[0]->type = e0->type;

                        iexps->remove(pos);
                        iexps->insert(pos, exps);
                        goto Lexpand1;
                    }
                }
            }
            if (iexps->dim < nelems)
                goto Lnomatch;

            ie = new TupleExp(_init->loc, iexps);
        }
Lnomatch:

        if (ie && ie->op == TOKtuple)
        {
            TupleExp *te = (TupleExp *)ie;
            size_t tedim = te->exps->dim;
            if (tedim != nelems)
            {
                ::error(loc, "tuple of %d elements cannot be assigned to tuple of %d elements", (int)tedim, (int)nelems);
                for (size_t u = tedim; u < nelems; u++) // fill dummy expression
                    te->exps->push(new ErrorExp());
            }
        }

        Objects *exps = new Objects();
        exps->setDim(nelems);
        for (size_t i = 0; i < nelems; i++)
        {
            Parameter *arg = Parameter::getNth(tt->arguments, i);

            OutBuffer buf;
            buf.printf("__%s_field_%llu", ident->toChars(), (ulonglong)i);
            const char *name = buf.extractString();
            Identifier *id = Identifier::idPool(name);

            Initializer *ti;
            if (ie)
            {
                Expression *einit = ie;
                if (ie->op == TOKtuple)
                {
                    TupleExp *te = (TupleExp *)ie;
                    einit = (*te->exps)[i];
                    if (i == 0)
                        einit = Expression::combine(te->e0, einit);
                }
                ti = new ExpInitializer(einit->loc, einit);
            }
            else
                ti = _init ? _init->syntaxCopy() : NULL;

            VarDeclaration *v = new VarDeclaration(loc, arg->type, id, ti);
            v->storage_class |= STCtemp | storage_class;
            if (arg->storageClass & STCparameter)
                v->storage_class |= arg->storageClass;
            //printf("declaring field %s of type %s\n", v->toChars(), v->type->toChars());
            v->semantic(sc);

            if (sc->scopesym)
            {
                //printf("adding %s to %s\n", v->toChars(), sc->scopesym->toChars());
                if (sc->scopesym->members)
                    sc->scopesym->members->push(v);
            }

            Expression *e = new DsymbolExp(loc, v);
            (*exps)[i] = e;
        }
        TupleDeclaration *v2 = new TupleDeclaration(loc, ident, exps);
        v2->parent = this->parent;
        v2->isexp = true;
        aliassym = v2;
        semanticRun = PASSsemanticdone;
        return;
    }

    /* Storage class can modify the type
     */
    type = type->addStorageClass(storage_class);

    /* Adjust storage class to reflect type
     */
    if (type->isConst())
    {
        storage_class |= STCconst;
        if (type->isShared())
            storage_class |= STCshared;
    }
    else if (type->isImmutable())
        storage_class |= STCimmutable;
    else if (type->isShared())
        storage_class |= STCshared;
    else if (type->isWild())
        storage_class |= STCwild;

    if (StorageClass stc = storage_class & (STCsynchronized | STCoverride | STCabstract | STCfinal))
    {
        if (stc == STCfinal)
            error("cannot be final, perhaps you meant const?");
        else
        {
            OutBuffer buf;
            stcToBuffer(&buf, stc);
            error("cannot be %s", buf.peekString());
        }
        storage_class &= ~stc;  // strip off
    }

    if (storage_class & STCscope)
    {
        StorageClass stc = storage_class & (STCstatic | STCextern | STCmanifest | STCtls | STCgshared);
        if (stc)
        {
            OutBuffer buf;
            stcToBuffer(&buf, stc);
            error("cannot be 'scope' and '%s'", buf.peekString());
        }
        else if (isMember())
        {
            error("field cannot be 'scope'");
        }
        else if (!type->hasPointers())
        {
            storage_class &= ~STCscope;     // silently ignore; may occur in generic code
        }
    }

    if (storage_class & (STCstatic | STCextern | STCmanifest | STCtemplateparameter | STCtls | STCgshared | STCctfe))
    {
    }
    else
    {
        AggregateDeclaration *aad = parent->isAggregateDeclaration();
        if (aad)
        {
            if (global.params.vfield &&
                storage_class & (STCconst | STCimmutable) && _init && !_init->isVoidInitializer())
            {
                const char *s = (storage_class & STCimmutable) ? "immutable" : "const";
                message(loc, "`%s.%s` is `%s` field", ad->toPrettyChars(), toChars(), s);
            }
            storage_class |= STCfield;
            if (tbn->ty == Tstruct && ((TypeStruct *)tbn)->sym->noDefaultCtor)
            {
                if (!isThisDeclaration() && !_init)
                    aad->noDefaultCtor = true;
            }
        }

        InterfaceDeclaration *id = parent->isInterfaceDeclaration();
        if (id)
        {
            error("field not allowed in interface");
        }
        else if (aad && aad->sizeok == SIZEOKdone)
        {
            error("cannot be further field because it will change the determined %s size", aad->toChars());
        }

        /* Templates cannot add fields to aggregates
         */
        TemplateInstance *ti = parent->isTemplateInstance();
        if (ti)
        {
            // Take care of nested templates
            while (1)
            {
                TemplateInstance *ti2 = ti->tempdecl->parent->isTemplateInstance();
                if (!ti2)
                    break;
                ti = ti2;
            }

            // If it's a member template
            AggregateDeclaration *ad2 = ti->tempdecl->isMember();
            if (ad2 && storage_class != STCundefined)
            {
                error("cannot use template to add field to aggregate '%s'", ad2->toChars());
            }
        }
    }

    if ((storage_class & (STCref | STCparameter | STCforeach | STCtemp | STCresult)) == STCref && ident != Id::This)
    {
        error("only parameters or foreach declarations can be ref");
    }

    if (type->hasWild())
    {
        if (storage_class & (STCstatic | STCextern | STCtls | STCgshared | STCmanifest | STCfield) ||
            isDataseg()
            )
        {
            error("only parameters or stack based variables can be inout");
        }
        FuncDeclaration *func = sc->func;
        if (func)
        {
            if (func->fes)
                func = func->fes->func;
            bool isWild = false;
            for (FuncDeclaration *fd = func; fd; fd = fd->toParent2()->isFuncDeclaration())
            {
                if (((TypeFunction *)fd->type)->iswild)
                {
                    isWild = true;
                    break;
                }
            }
            if (!isWild)
            {
                error("inout variables can only be declared inside inout functions");
            }
        }
    }

    if (!(storage_class & (STCctfe | STCref | STCresult)) && tbn->ty == Tstruct &&
        ((TypeStruct *)tbn)->sym->noDefaultCtor)
    {
        if (!_init)
        {
            if (isField())
            {
                /* For fields, we'll check the constructor later to make sure it is initialized
                 */
                storage_class |= STCnodefaultctor;
            }
            else if (storage_class & STCparameter)
                ;
            else
                error("default construction is disabled for type %s", type->toChars());
        }
    }

    FuncDeclaration *fd = parent->isFuncDeclaration();
    if (type->isscope() && !(storage_class & STCnodtor))
    {
        if (storage_class & (STCfield | STCout | STCref | STCstatic | STCmanifest | STCtls | STCgshared) || !fd)
        {
            error("globals, statics, fields, manifest constants, ref and out parameters cannot be scope");
        }

        if (!(storage_class & STCscope))
        {
            if (!(storage_class & STCparameter) && ident != Id::withSym)
                error("reference to scope class must be scope");
        }
    }

    // Calculate type size + safety checks
    if (sc->func && !sc->intypeof)
    {
        if (_init && _init->isVoidInitializer() && type->hasPointers()) // get type size
        {
            if (sc->func->setUnsafe())
                error("void initializers for pointers not allowed in safe functions");
        }
        else if (!_init &&
                 !(storage_class & (STCstatic | STCextern | STCtls | STCgshared | STCmanifest | STCfield | STCparameter)) &&
                 type->hasVoidInitPointers())
        {
            if (sc->func->setUnsafe())
                error("void initializers for pointers not allowed in safe functions");
        }
    }

    if (!_init && !fd)
    {
        // If not mutable, initializable by constructor only
        storage_class |= STCctorinit;
    }

    if (_init)
        storage_class |= STCinit;     // remember we had an explicit initializer
    else if (storage_class & STCmanifest)
        error("manifest constants must have initializers");

    bool isBlit = false;
    d_uns64 sz = 0;
    if (!_init && !sc->inunion && !(storage_class & (STCstatic | STCgshared | STCextern)) && fd &&
        (!(storage_class & (STCfield | STCin | STCforeach | STCparameter | STCresult))
         || (storage_class & STCout)) &&
        (sz = type->size()) != 0)
    {
        // Provide a default initializer
        //printf("Providing default initializer for '%s'\n", toChars());
        if (sz == SIZE_INVALID && type->ty != Terror)
            error("size of type %s is invalid", type->toChars());

        Type *tv = type;
        while (tv->ty == Tsarray)    // Don't skip Tenum
            tv = tv->nextOf();
        if (tv->needsNested())
        {
            /* Nested struct requires valid enclosing frame pointer.
             * In StructLiteralExp::toElem(), it's calculated.
             */
            assert(tv->toBasetype()->ty == Tstruct);
            checkFrameAccess(loc, sc, ((TypeStruct *)tbn)->sym);

            Expression *e = tv->defaultInitLiteral(loc);
            e = new BlitExp(loc, new VarExp(loc, this), e);
            e = ::semantic(e, sc);
            _init = new ExpInitializer(loc, e);
            goto Ldtor;
        }
        if (tv->ty == Tstruct && ((TypeStruct *)tv)->sym->zeroInit == 1)
        {
            /* If a struct is all zeros, as a special case
             * set it's initializer to the integer 0.
             * In AssignExp::toElem(), we check for this and issue
             * a memset() to initialize the struct.
             * Must do same check in interpreter.
             */
            Expression *e = new IntegerExp(loc, 0, Type::tint32);
            e = new BlitExp(loc, new VarExp(loc, this), e);
            e->type = type;         // don't type check this, it would fail
            _init = new ExpInitializer(loc, e);
            goto Ldtor;
        }
        if (type->baseElemOf()->ty == Tvoid)
        {
            error("%s does not have a default initializer", type->toChars());
        }
        else if (Expression *e = type->defaultInit(loc))
        {
            _init = new ExpInitializer(loc, e);
        }
        // Default initializer is always a blit
        isBlit = true;
    }

    if (_init)
    {
        sc = sc->push();
        sc->stc &= ~(STC_TYPECTOR | STCpure | STCnothrow | STCnogc | STCref | STCdisable);

        ExpInitializer *ei = _init->isExpInitializer();
        if (ei)     // Bugzilla 13424: Preset the required type to fail in FuncLiteralDeclaration::semantic3
            ei->exp = inferType(ei->exp, type);

        // If inside function, there is no semantic3() call
        if (sc->func || sc->intypeof == 1)
        {
            // If local variable, use AssignExp to handle all the various
            // possibilities.
            if (fd &&
                !(storage_class & (STCmanifest | STCstatic | STCtls | STCgshared | STCextern)) &&
                !_init->isVoidInitializer())
            {
                //printf("fd = '%s', var = '%s'\n", fd->toChars(), toChars());
                if (!ei)
                {
                    ArrayInitializer *ai = _init->isArrayInitializer();
                    Expression *e;
                    if (ai && tb->ty == Taarray)
                        e = ai->toAssocArrayLiteral();
                    else
                        e = initializerToExpression(_init);
                    if (!e)
                    {
                        // Run semantic, but don't need to interpret
                        _init = ::semantic(_init, sc, type, INITnointerpret);
                        e = initializerToExpression(_init);
                        if (!e)
                        {
                            error("is not a static and cannot have static initializer");
                            return;
                        }
                    }
                    ei = new ExpInitializer(_init->loc, e);
                    _init = ei;
                }

                Expression *exp = ei->exp;
                Expression *e1 = new VarExp(loc, this);
                if (isBlit)
                    exp = new BlitExp(loc, e1, exp);
                else
                    exp = new ConstructExp(loc, e1, exp);
                canassign++;
                exp = ::semantic(exp, sc);
                canassign--;
                exp = exp->optimize(WANTvalue);

                if (exp->op == TOKerror)
                {
                    _init = new ErrorInitializer();
                    ei = NULL;
                }
                else
                    ei->exp = exp;

                if (ei && isScope())
                {
                    Expression *ex = ei->exp;
                    while (ex->op == TOKcomma)
                        ex = ((CommaExp *)ex)->e2;
                    if (ex->op == TOKblit || ex->op == TOKconstruct)
                        ex = ((AssignExp *)ex)->e2;
                    if (ex->op == TOKnew)
                    {
                        // See if initializer is a NewExp that can be allocated on the stack
                        NewExp *ne = (NewExp *)ex;
                        if (type->toBasetype()->ty == Tclass)
                        {
                            if (ne->newargs && ne->newargs->dim > 1)
                            {
                                mynew = true;
                            }
                            else
                            {
                                ne->onstack = true;
                                onstack = true;
                            }
                        }
                    }
                    else if (ex->op == TOKfunction)
                    {
                        // or a delegate that doesn't escape a reference to the function
                        FuncDeclaration *f = ((FuncExp *)ex)->fd;
                        f->tookAddressOf--;
                    }
                }
            }
            else
            {
                // Bugzilla 14166: Don't run CTFE for the temporary variables inside typeof
                _init = ::semantic(_init, sc, type, sc->intypeof == 1 ? INITnointerpret : INITinterpret);
            }
        }
        else if (parent->isAggregateDeclaration())
        {
            _scope = scx ? scx : sc->copy();
            _scope->setNoFree();
        }
        else if (storage_class & (STCconst | STCimmutable | STCmanifest) ||
                 type->isConst() || type->isImmutable())
        {
            /* Because we may need the results of a const declaration in a
             * subsequent type, such as an array dimension, before semantic2()
             * gets ordinarily run, try to run semantic2() now.
             * Ignore failure.
             */

            if (!inferred)
            {
                unsigned errors = global.errors;
                inuse++;
                if (ei)
                {
                    Expression *exp = ei->exp->syntaxCopy();

                    bool needctfe = isDataseg() || (storage_class & STCmanifest);
                    if (needctfe) sc = sc->startCTFE();
                    exp = ::semantic(exp, sc);
                    exp = resolveProperties(sc, exp);
                    if (needctfe) sc = sc->endCTFE();

                    Type *tb2 = type->toBasetype();
                    Type *ti = exp->type->toBasetype();

                    /* The problem is the following code:
                     *  struct CopyTest {
                     *     double x;
                     *     this(double a) { x = a * 10.0;}
                     *     this(this) { x += 2.0; }
                     *  }
                     *  const CopyTest z = CopyTest(5.3);  // ok
                     *  const CopyTest w = z;              // not ok, postblit not run
                     *  static assert(w.x == 55.0);
                     * because the postblit doesn't get run on the initialization of w.
                     */
                    if (ti->ty == Tstruct)
                    {
                        StructDeclaration *sd = ((TypeStruct *)ti)->sym;
                        /* Look to see if initializer involves a copy constructor
                         * (which implies a postblit)
                         */
                         // there is a copy constructor
                         // and exp is the same struct
                        if (sd->postblit &&
                            tb2->toDsymbol(NULL) == sd)
                        {
                            // The only allowable initializer is a (non-copy) constructor
                            if (exp->isLvalue())
                                error("of type struct %s uses this(this), which is not allowed in static initialization", tb2->toChars());
                        }
                    }
                    ei->exp = exp;
                }
                _init = ::semantic(_init, sc, type, INITinterpret);
                inuse--;
                if (global.errors > errors)
                {
                    _init = new ErrorInitializer();
                    type = Type::terror;
                }
            }
            else
            {
                _scope = scx ? scx : sc->copy();
                _scope->setNoFree();
            }
        }
        sc = sc->pop();
    }

Ldtor:
    /* Build code to execute destruction, if necessary
     */
    edtor = callScopeDtor(sc);
    if (edtor)
    {
        if (sc->func && storage_class & (STCstatic | STCgshared))
            edtor = ::semantic(edtor, sc->_module->_scope);
        else
            edtor = ::semantic(edtor, sc);

#if 0 // currently disabled because of std.stdio.stdin, stdout and stderr
        if (isDataseg() && !(storage_class & STCextern))
            error("static storage variables cannot have destructors");
#endif
    }

    semanticRun = PASSsemanticdone;

    if (type->toBasetype()->ty == Terror)
        errors = true;

    if (sc->scopesym && !sc->scopesym->isAggregateDeclaration())
    {
        for (ScopeDsymbol *sym = sc->scopesym; sym && endlinnum == 0;
             sym = sym->parent ? sym->parent->isScopeDsymbol() : NULL)
            endlinnum = sym->endlinnum;
    }
}

void VarDeclaration::semantic2(Scope *sc)
{
    if (semanticRun < PASSsemanticdone && inuse)
        return;

    //printf("VarDeclaration::semantic2('%s')\n", toChars());

    if (_init && !toParent()->isFuncDeclaration())
    {
        inuse++;
        // Bugzilla 14166: Don't run CTFE for the temporary variables inside typeof
        _init = ::semantic(_init, sc, type, sc->intypeof == 1 ? INITnointerpret : INITinterpret);
        inuse--;
    }
    if (_init && storage_class & STCmanifest)
    {
        /* Cannot initializer enums with CTFE classreferences and addresses of struct literals.
         * Scan initializer looking for them. Issue error if found.
         */
        if (ExpInitializer *ei = _init->isExpInitializer())
        {
            struct EnumInitializer
            {
                static bool arrayHasInvalidEnumInitializer(Expressions *elems)
                {
                    for (size_t i = 0; i < elems->dim; i++)
                    {
                        Expression *e = (*elems)[i];
                        if (e && hasInvalidEnumInitializer(e))
                            return true;
                    }
                    return false;
                }

                static bool hasInvalidEnumInitializer(Expression *e)
                {
                    if (e->op == TOKclassreference)
                        return true;
                    if (e->op == TOKaddress && ((AddrExp *)e)->e1->op == TOKstructliteral)
                        return true;
                    if (e->op == TOKarrayliteral)
                        return arrayHasInvalidEnumInitializer(((ArrayLiteralExp *)e)->elements);
                    if (e->op == TOKstructliteral)
                        return arrayHasInvalidEnumInitializer(((StructLiteralExp *)e)->elements);
                    if (e->op == TOKassocarrayliteral)
                    {
                        AssocArrayLiteralExp *ae = (AssocArrayLiteralExp *)e;
                        return arrayHasInvalidEnumInitializer(ae->values) ||
                            arrayHasInvalidEnumInitializer(ae->keys);
                    }
                    return false;
                }
            };
            if (EnumInitializer::hasInvalidEnumInitializer(ei->exp))
                error(": Unable to initialize enum with class or pointer to struct. Use static const variable instead.");
        }
    }
    else if (_init && isThreadlocal())
    {
        if ((type->ty == Tclass) && type->isMutable() && !type->isShared())
        {
            ExpInitializer *ei = _init->isExpInitializer();
            if (ei && ei->exp->op == TOKclassreference)
                error("is mutable. Only const or immutable class thread local variable are allowed, not %s", type->toChars());
        }
        else if (type->ty == Tpointer && type->nextOf()->ty == Tstruct && type->nextOf()->isMutable() &&!type->nextOf()->isShared())
        {
            ExpInitializer *ei = _init->isExpInitializer();
            if (ei && ei->exp->op == TOKaddress && ((AddrExp *)ei->exp)->e1->op == TOKstructliteral)
            {
                error("is a pointer to mutable struct. Only pointers to const, immutable or shared struct thread local variable are allowed, not %s", type->toChars());
            }
        }
    }
    semanticRun = PASSsemantic2done;
}

void VarDeclaration::setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion)
{
    //printf("VarDeclaration::setFieldOffset(ad = %s) %s\n", ad->toChars(), toChars());

    if (aliassym)
    {
        // If this variable was really a tuple, set the offsets for the tuple fields
        TupleDeclaration *v2 = aliassym->isTupleDeclaration();
        assert(v2);
        for (size_t i = 0; i < v2->objects->dim; i++)
        {
            RootObject *o = (*v2->objects)[i];
            assert(o->dyncast() == DYNCAST_EXPRESSION);
            Expression *e = (Expression *)o;
            assert(e->op == TOKdsymbol);
            DsymbolExp *se = (DsymbolExp *)e;
            se->s->setFieldOffset(ad, poffset, isunion);
        }
        return;
    }

    if (!isField())
        return;
    assert(!(storage_class & (STCstatic | STCextern | STCparameter | STCtls)));

    //printf("+VarDeclaration::setFieldOffset(ad = %s) %s\n", ad->toChars(), toChars());

    /* Fields that are tuples appear both as part of TupleDeclarations and
     * as members. That means ignore them if they are already a field.
     */
    if (offset)
    {
        // already a field
        *poffset = ad->structsize;  // Bugzilla 13613
        return;
    }
    for (size_t i = 0; i < ad->fields.dim; i++)
    {
        if (ad->fields[i] == this)
        {
            // already a field
            *poffset = ad->structsize;  // Bugzilla 13613
            return;
        }
    }

    // Check for forward referenced types which will fail the size() call
    Type *t = type->toBasetype();
    if (storage_class & STCref)
    {
        // References are the size of a pointer
        t = Type::tvoidptr;
    }
    Type *tv = t->baseElemOf();
    if (tv->ty == Tstruct)
    {
        TypeStruct *ts = (TypeStruct *)tv;
        assert(ts->sym != ad);   // already checked in ad->determineFields()
        if (!ts->sym->determineSize(loc))
        {
            type = Type::terror;
            errors = true;
            return;
        }
    }

    // List in ad->fields. Even if the type is error, it's necessary to avoid
    // pointless error diagnostic "more initializers than fields" on struct literal.
    ad->fields.push(this);

    if (t->ty == Terror)
        return;

    const d_uns64 sz = t->size(loc);
    assert(sz != SIZE_INVALID && sz < UINT32_MAX);
    unsigned memsize = (unsigned)sz;                 // size of member
    unsigned memalignsize = Target::fieldalign(t);   // size of member for alignment purposes

    offset = AggregateDeclaration::placeField(poffset, memsize, memalignsize, alignment,
                &ad->structsize, &ad->alignsize, isunion);

    //printf("\t%s: memalignsize = %d\n", toChars(), memalignsize);

    //printf(" addField '%s' to '%s' at offset %d, size = %d\n", toChars(), ad->toChars(), offset, memsize);
}

const char *VarDeclaration::kind() const
{
    return "variable";
}

Dsymbol *VarDeclaration::toAlias()
{
    //printf("VarDeclaration::toAlias('%s', this = %p, aliassym = %p)\n", toChars(), this, aliassym);
    if ((!type || !type->deco) && _scope)
        semantic(_scope);

    assert(this != aliassym);
    Dsymbol *s = aliassym ? aliassym->toAlias() : this;
    return s;
}

AggregateDeclaration *VarDeclaration::isThis()
{
    AggregateDeclaration *ad = NULL;

    if (!(storage_class & (STCstatic | STCextern | STCmanifest | STCtemplateparameter |
                           STCtls | STCgshared | STCctfe)))
    {
        for (Dsymbol *s = this; s; s = s->parent)
        {
            ad = s->isMember();
            if (ad)
                break;
            if (!s->parent || !s->parent->isTemplateMixin()) break;
        }
    }
    return ad;
}

bool VarDeclaration::needThis()
{
    //printf("VarDeclaration::needThis(%s, x%x)\n", toChars(), storage_class);
    return isField();
}

bool VarDeclaration::isExport() const
{
    return protection.kind == PROTexport;
}

bool VarDeclaration::isImportedSymbol() const
{
    if (protection.kind == PROTexport && !_init &&
        (storage_class & STCstatic || parent->isModule()))
        return true;
    return false;
}

/*******************************************
 * Helper function for the expansion of manifest constant.
 */
Expression *VarDeclaration::expandInitializer(Loc loc)
{
    assert((storage_class & STCmanifest) && _init);

    Expression *e = getConstInitializer();
    if (!e)
    {
        ::error(loc, "cannot make expression out of initializer for %s", toChars());
        return new ErrorExp();
    }

    e = e->copy();
    e->loc = loc;    // for better error message
    return e;
}

void VarDeclaration::checkCtorConstInit()
{
#if 0 /* doesn't work if more than one static ctor */
    if (ctorinit == 0 && isCtorinit() && !isField())
        error("missing initializer in static constructor for const variable");
#endif
}

bool lambdaCheckForNestedRef(Expression *e, Scope *sc);

/************************************
 * Check to see if this variable is actually in an enclosing function
 * rather than the current one.
 * Returns true if error occurs.
 */
bool VarDeclaration::checkNestedReference(Scope *sc, Loc loc)
{
    //printf("VarDeclaration::checkNestedReference() %s\n", toChars());
    if (sc->intypeof == 1 || (sc->flags & SCOPEctfe))
        return false;
    if (!parent || parent == sc->parent)
        return false;
    if (isDataseg() || (storage_class & STCmanifest))
        return false;

    // The current function
    FuncDeclaration *fdthis = sc->parent->isFuncDeclaration();
    if (!fdthis)
        return false;   // out of function scope

    Dsymbol *p = toParent2();

    // Function literals from fdthis to p must be delegates
    checkNestedRef(fdthis, p);

    // The function that this variable is in
    FuncDeclaration *fdv = p->isFuncDeclaration();
    if (!fdv || fdv == fdthis)
        return false;

    // Add fdthis to nestedrefs[] if not already there
    for (size_t i = 0; 1; i++)
    {
        if (i == nestedrefs.dim)
        {
            nestedrefs.push(fdthis);
            break;
        }
        if (nestedrefs[i] == fdthis)
            break;
    }

    /* __require and __ensure will always get called directly,
     * so they never make outer functions closure.
     */
    if (fdthis->ident == Id::require || fdthis->ident == Id::ensure)
        return false;

    //printf("\tfdv = %s\n", fdv->toChars());
    //printf("\tfdthis = %s\n", fdthis->toChars());
    if (loc.filename)
    {
        int lv = fdthis->getLevel(loc, sc, fdv);
        if (lv == -2)   // error
            return true;
    }

    // Add this to fdv->closureVars[] if not already there
    for (size_t i = 0; 1; i++)
    {
        if (i == fdv->closureVars.dim)
        {
            if (!sc->intypeof && !(sc->flags & SCOPEcompile))
                fdv->closureVars.push(this);
            break;
        }
        if (fdv->closureVars[i] == this)
            break;
    }

    //printf("fdthis is %s\n", fdthis->toChars());
    //printf("var %s in function %s is nested ref\n", toChars(), fdv->toChars());
    // __dollar creates problems because it isn't a real variable Bugzilla 3326
    if (ident == Id::dollar)
    {
        ::error(loc, "cannnot use $ inside a function literal");
        return true;
    }

    if (ident == Id::withSym)       // Bugzilla 1759
    {
        ExpInitializer *ez = _init->isExpInitializer();
        assert(ez);
        Expression *e = ez->exp;
        if (e->op == TOKconstruct || e->op == TOKblit)
            e = ((AssignExp *)e)->e2;
        return lambdaCheckForNestedRef(e, sc);
    }

    return false;
}

/*******************************************
 * If variable has a constant expression initializer, get it.
 * Otherwise, return NULL.
 */

Expression *VarDeclaration::getConstInitializer(bool needFullType)
{
    assert(type && _init);

    // Ungag errors when not speculative
    unsigned oldgag = global.gag;
    if (global.gag)
    {
        Dsymbol *sym = toParent()->isAggregateDeclaration();
        if (sym && !sym->isSpeculative())
            global.gag = 0;
    }

    if (_scope)
    {
        inuse++;
        _init = ::semantic(_init, _scope, type, INITinterpret);
        _scope = NULL;
        inuse--;
    }
    Expression *e = initializerToExpression(_init, needFullType ? type : NULL);

    global.gag = oldgag;
    return e;
}

/*************************************
 * Return true if we can take the address of this variable.
 */

bool VarDeclaration::canTakeAddressOf()
{
    return !(storage_class & STCmanifest);
}


/*******************************
 * Does symbol go into data segment?
 * Includes extern variables.
 */

bool VarDeclaration::isDataseg()
{
    if (isdataseg == 0) // the value is not cached
    {
        isdataseg = 2; // The Variables does not go into the datasegment

        if (!canTakeAddressOf())
        {
            return false;
        }

        Dsymbol *parent = toParent();
        if (!parent && !(storage_class & STCstatic))
        {
            error("forward referenced");
            type = Type::terror;
        }
        else if (storage_class & (STCstatic | STCextern | STCtls | STCgshared) ||
                 parent->isModule() || parent->isTemplateInstance() || parent->isNspace())
        {
            assert(!isParameter() && !isResult());
            isdataseg = 1; // It is in the DataSegment
        }
    }

    return (isdataseg == 1);
}

/************************************
 * Does symbol go into thread local storage?
 */

bool VarDeclaration::isThreadlocal()
{
    //printf("VarDeclaration::isThreadlocal(%p, '%s')\n", this, toChars());
    /* Data defaults to being thread-local. It is not thread-local
     * if it is immutable, const or shared.
     */
    bool i = isDataseg() &&
        !(storage_class & (STCimmutable | STCconst | STCshared | STCgshared));
    //printf("\treturn %d\n", i);
    return i;
}

/********************************************
 * Can variable be read and written by CTFE?
 */

bool VarDeclaration::isCTFE()
{
    return (storage_class & STCctfe) != 0; // || !isDataseg();
}

bool VarDeclaration::isOverlappedWith(VarDeclaration *v)
{
    const d_uns64 vsz = v->type->size();
    const d_uns64 tsz = type->size();
    assert(vsz != SIZE_INVALID && tsz != SIZE_INVALID);
    return offset < v->offset + vsz &&
        v->offset < offset + tsz;
}

bool VarDeclaration::hasPointers()
{
    //printf("VarDeclaration::hasPointers() %s, ty = %d\n", toChars(), type->ty);
    return (!isDataseg() && type->hasPointers());
}

/******************************************
 * Return true if variable needs to call the destructor.
 */

bool VarDeclaration::needsScopeDtor()
{
    //printf("VarDeclaration::needsScopeDtor() %s\n", toChars());
    return edtor && !(storage_class & STCnodtor);
}


/******************************************
 * If a variable has a scope destructor call, return call for it.
 * Otherwise, return NULL.
 */

Expression *VarDeclaration::callScopeDtor(Scope *)
{
    //printf("VarDeclaration::callScopeDtor() %s\n", toChars());

    // Destruction of STCfield's is handled by buildDtor()
    if (storage_class & (STCnodtor | STCref | STCout | STCfield))
    {
        return NULL;
    }

    Expression *e = NULL;

    // Destructors for structs and arrays of structs
    Type *tv = type->baseElemOf();
    if (tv->ty == Tstruct)
    {
        StructDeclaration *sd = ((TypeStruct *)tv)->sym;
        if (!sd->dtor || sd->errors)
           return NULL;

        const d_uns64 sz = type->size();
        assert(sz != SIZE_INVALID);
        if (!sz)
            return NULL;

        if (type->toBasetype()->ty == Tstruct)
        {
            // v.__xdtor()
            e = new VarExp(loc, this);

            /* This is a hack so we can call destructors on const/immutable objects.
             * Need to add things like "const ~this()" and "immutable ~this()" to
             * fix properly.
             */
            e->type = e->type->mutableOf();

            // Enable calling destructors on shared objects.
            // The destructor is always a single, non-overloaded function,
            // and must serve both shared and non-shared objects.
            e->type = e->type->unSharedOf();

            e = new DotVarExp(loc, e, sd->dtor, false);
            e = new CallExp(loc, e);
        }
        else
        {
            // __ArrayDtor(v[0 .. n])
            e = new VarExp(loc, this);

            const d_uns64 sdsz = sd->type->size();
            assert(sdsz != SIZE_INVALID && sdsz != 0);
            const d_uns64 n = sz / sdsz;
            e = new SliceExp(loc, e, new IntegerExp(loc, 0, Type::tsize_t),
                                     new IntegerExp(loc, n, Type::tsize_t));
            // Prevent redundant bounds check
            ((SliceExp *)e)->upperIsInBounds = true;
            ((SliceExp *)e)->lowerIsLessThanUpper = true;

            // This is a hack so we can call destructors on const/immutable objects.
            e->type = sd->type->arrayOf();

            e = new CallExp(loc, new IdentifierExp(loc, Id::__ArrayDtor), e);
        }
        return e;
    }

    // Destructors for classes
    if (storage_class & (STCauto | STCscope) && !(storage_class & STCparameter))
    {
        for (ClassDeclaration *cd = type->isClassHandle();
             cd;
             cd = cd->baseClass)
        {
            /* We can do better if there's a way with onstack
             * classes to determine if there's no way the monitor
             * could be set.
             */
            //if (cd->isInterfaceDeclaration())
                //error("interface %s cannot be scope", cd->toChars());

            // Destroying C++ scope classes crashes currently. Since C++ class dtors are not currently supported, simply do not run dtors for them.
            // See https://issues.dlang.org/show_bug.cgi?id=13182
            if (cd->cpp)
            {
                break;
            }
            if (mynew || onstack) // if any destructors
            {
                // delete this;
                Expression *ec;

                ec = new VarExp(loc, this);
                e = new DeleteExp(loc, ec, true);
                e->type = Type::tvoid;
                break;
            }
        }
    }
    return e;
}

/**********************************
 * Determine if `this` has a lifetime that lasts past
 * the destruction of `v`
 * Params:
 *  v = variable to test against
 * Returns:
 *  true if it does
 */
bool VarDeclaration::enclosesLifetimeOf(VarDeclaration *v) const
{
    return sequenceNumber < v->sequenceNumber;
}

/******************************************
 */

void ObjectNotFound(Identifier *id)
{
    Type::error(Loc(), "%s not found. object.d may be incorrectly installed or corrupt.", id->toChars());
    fatal();
}

/******************************** SymbolDeclaration ********************************/

SymbolDeclaration::SymbolDeclaration(Loc loc, StructDeclaration *dsym)
        : Declaration(dsym->ident)
{
    this->loc = loc;
    this->dsym = dsym;
    storage_class |= STCconst;
}

/********************************* TypeInfoDeclaration ****************************/

TypeInfoDeclaration::TypeInfoDeclaration(Type *tinfo)
    : VarDeclaration(Loc(), Type::dtypeinfo->type, tinfo->getTypeInfoIdent(), NULL)
{
    this->tinfo = tinfo;
    storage_class = STCstatic | STCgshared;
    protection = Prot(PROTpublic);
    linkage = LINKc;
    alignment = Target::ptrsize;
}

TypeInfoDeclaration *TypeInfoDeclaration::create(Type *tinfo)
{
    return new TypeInfoDeclaration(tinfo);
}

Dsymbol *TypeInfoDeclaration::syntaxCopy(Dsymbol *)
{
    assert(0);          // should never be produced by syntax
    return NULL;
}

void TypeInfoDeclaration::semantic(Scope *)
{
    assert(linkage == LINKc);
}

const char *TypeInfoDeclaration::toChars()
{
    //printf("TypeInfoDeclaration::toChars() tinfo = %s\n", tinfo->toChars());
    OutBuffer buf;
    buf.writestring("typeid(");
    buf.writestring(tinfo->toChars());
    buf.writeByte(')');
    return buf.extractString();
}

/***************************** TypeInfoConstDeclaration **********************/

TypeInfoConstDeclaration::TypeInfoConstDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoconst)
    {
        ObjectNotFound(Id::TypeInfo_Const);
    }
    type = Type::typeinfoconst->type;
}

TypeInfoConstDeclaration *TypeInfoConstDeclaration::create(Type *tinfo)
{
    return new TypeInfoConstDeclaration(tinfo);
}

/***************************** TypeInfoInvariantDeclaration **********************/

TypeInfoInvariantDeclaration::TypeInfoInvariantDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoinvariant)
    {
        ObjectNotFound(Id::TypeInfo_Invariant);
    }
    type = Type::typeinfoinvariant->type;
}

TypeInfoInvariantDeclaration *TypeInfoInvariantDeclaration::create(Type *tinfo)
{
    return new TypeInfoInvariantDeclaration(tinfo);
}

/***************************** TypeInfoSharedDeclaration **********************/

TypeInfoSharedDeclaration::TypeInfoSharedDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoshared)
    {
        ObjectNotFound(Id::TypeInfo_Shared);
    }
    type = Type::typeinfoshared->type;
}

TypeInfoSharedDeclaration *TypeInfoSharedDeclaration::create(Type *tinfo)
{
    return new TypeInfoSharedDeclaration(tinfo);
}

/***************************** TypeInfoWildDeclaration **********************/

TypeInfoWildDeclaration::TypeInfoWildDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfowild)
    {
        ObjectNotFound(Id::TypeInfo_Wild);
    }
    type = Type::typeinfowild->type;
}

TypeInfoWildDeclaration *TypeInfoWildDeclaration::create(Type *tinfo)
{
    return new TypeInfoWildDeclaration(tinfo);
}

/***************************** TypeInfoStructDeclaration **********************/

TypeInfoStructDeclaration::TypeInfoStructDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfostruct)
    {
        ObjectNotFound(Id::TypeInfo_Struct);
    }
    type = Type::typeinfostruct->type;
}

TypeInfoStructDeclaration *TypeInfoStructDeclaration::create(Type *tinfo)
{
    return new TypeInfoStructDeclaration(tinfo);
}

/***************************** TypeInfoClassDeclaration ***********************/

TypeInfoClassDeclaration::TypeInfoClassDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoclass)
    {
        ObjectNotFound(Id::TypeInfo_Class);
    }
    type = Type::typeinfoclass->type;
}

TypeInfoClassDeclaration *TypeInfoClassDeclaration::create(Type *tinfo)
{
    return new TypeInfoClassDeclaration(tinfo);
}

/***************************** TypeInfoInterfaceDeclaration *******************/

TypeInfoInterfaceDeclaration::TypeInfoInterfaceDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfointerface)
    {
        ObjectNotFound(Id::TypeInfo_Interface);
    }
    type = Type::typeinfointerface->type;
}

TypeInfoInterfaceDeclaration *TypeInfoInterfaceDeclaration::create(Type *tinfo)
{
    return new TypeInfoInterfaceDeclaration(tinfo);
}

/***************************** TypeInfoPointerDeclaration *********************/

TypeInfoPointerDeclaration::TypeInfoPointerDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfopointer)
    {
        ObjectNotFound(Id::TypeInfo_Pointer);
    }
    type = Type::typeinfopointer->type;
}

TypeInfoPointerDeclaration *TypeInfoPointerDeclaration::create(Type *tinfo)
{
    return new TypeInfoPointerDeclaration(tinfo);
}

/***************************** TypeInfoArrayDeclaration ***********************/

TypeInfoArrayDeclaration::TypeInfoArrayDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoarray)
    {
        ObjectNotFound(Id::TypeInfo_Array);
    }
    type = Type::typeinfoarray->type;
}

TypeInfoArrayDeclaration *TypeInfoArrayDeclaration::create(Type *tinfo)
{
    return new TypeInfoArrayDeclaration(tinfo);
}

/***************************** TypeInfoStaticArrayDeclaration *****************/

TypeInfoStaticArrayDeclaration::TypeInfoStaticArrayDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfostaticarray)
    {
        ObjectNotFound(Id::TypeInfo_StaticArray);
    }
    type = Type::typeinfostaticarray->type;
}

TypeInfoStaticArrayDeclaration *TypeInfoStaticArrayDeclaration::create(Type *tinfo)
{
    return new TypeInfoStaticArrayDeclaration(tinfo);
}

/***************************** TypeInfoAssociativeArrayDeclaration ************/

TypeInfoAssociativeArrayDeclaration::TypeInfoAssociativeArrayDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoassociativearray)
    {
        ObjectNotFound(Id::TypeInfo_AssociativeArray);
    }
    type = Type::typeinfoassociativearray->type;
}

TypeInfoAssociativeArrayDeclaration *TypeInfoAssociativeArrayDeclaration::create(Type *tinfo)
{
    return new TypeInfoAssociativeArrayDeclaration(tinfo);
}

/***************************** TypeInfoVectorDeclaration ***********************/

TypeInfoVectorDeclaration::TypeInfoVectorDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfovector)
    {
        ObjectNotFound(Id::TypeInfo_Vector);
    }
    type = Type::typeinfovector->type;
}

TypeInfoVectorDeclaration *TypeInfoVectorDeclaration::create(Type *tinfo)
{
    return new TypeInfoVectorDeclaration(tinfo);
}

/***************************** TypeInfoEnumDeclaration ***********************/

TypeInfoEnumDeclaration::TypeInfoEnumDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfoenum)
    {
        ObjectNotFound(Id::TypeInfo_Enum);
    }
    type = Type::typeinfoenum->type;
}

TypeInfoEnumDeclaration *TypeInfoEnumDeclaration::create(Type *tinfo)
{
    return new TypeInfoEnumDeclaration(tinfo);
}

/***************************** TypeInfoFunctionDeclaration ********************/

TypeInfoFunctionDeclaration::TypeInfoFunctionDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfofunction)
    {
        ObjectNotFound(Id::TypeInfo_Function);
    }
    type = Type::typeinfofunction->type;
}

TypeInfoFunctionDeclaration *TypeInfoFunctionDeclaration::create(Type *tinfo)
{
    return new TypeInfoFunctionDeclaration(tinfo);
}

/***************************** TypeInfoDelegateDeclaration ********************/

TypeInfoDelegateDeclaration::TypeInfoDelegateDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfodelegate)
    {
        ObjectNotFound(Id::TypeInfo_Delegate);
    }
    type = Type::typeinfodelegate->type;
}

TypeInfoDelegateDeclaration *TypeInfoDelegateDeclaration::create(Type *tinfo)
{
    return new TypeInfoDelegateDeclaration(tinfo);
}

/***************************** TypeInfoTupleDeclaration **********************/

TypeInfoTupleDeclaration::TypeInfoTupleDeclaration(Type *tinfo)
    : TypeInfoDeclaration(tinfo)
{
    if (!Type::typeinfotypelist)
    {
        ObjectNotFound(Id::TypeInfo_Tuple);
    }
    type = Type::typeinfotypelist->type;
}

TypeInfoTupleDeclaration *TypeInfoTupleDeclaration::create(Type *tinfo)
{
    return new TypeInfoTupleDeclaration(tinfo);
}

/********************************* ThisDeclaration ****************************/

// For the "this" parameter to member functions

ThisDeclaration::ThisDeclaration(Loc loc, Type *t)
   : VarDeclaration(loc, t, Id::This, NULL)
{
    storage_class |= STCnodtor;
}

Dsymbol *ThisDeclaration::syntaxCopy(Dsymbol *)
{
    assert(0);          // should never be produced by syntax
    return NULL;
}

