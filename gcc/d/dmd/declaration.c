
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
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
    for (size_t i = iStart; i < ad->fields.length; i++)
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
    protection = Prot(Prot::undefined);
    linkage = LINKdefault;
    inuse = 0;
    mangleOverride = NULL;
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
                if (!flag) error(loc, "cannot modify %s `%s` in contract", s, toChars());
                return 2;   // do not report type related errors
            }
        }
    }

    if (e1 && e1->op == TOKthis && isField())
    {
        VarDeclaration *vthis = e1->isThisExp()->var;
        for (Scope *scx = sc; scx; scx = scx->enclosing)
        {
            if (scx->func == vthis->parent && (scx->flags & SCOPEcontract))
            {
                if (!flag)
                    error(loc, "cannot modify parameter `this` in contract");
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

/**
 * Issue an error if an attempt to call a disabled method is made
 *
 * If the declaration is disabled but inside a disabled function,
 * returns `true` but do not issue an error message.
 *
 * Params:
 *   loc = Location information of the call
 *   sc  = Scope in which the call occurs
 *   isAliasedDeclaration = if `true` searches overload set
 *
 * Returns:
 *   `true` if this `Declaration` is `@disable`d, `false` otherwise.
 */
bool Declaration::checkDisabled(Loc loc, Scope *sc, bool isAliasedDeclaration)
{
    if (!(storage_class & STCdisable))
        return false;

    if (sc->func && (sc->func->storage_class & STCdisable))
        return true;

    Dsymbol *p = toParent();
    if (p && isPostBlitDeclaration())
    {
        p->error(loc, "is not copyable because it is annotated with `@disable`");
        return true;
    }

    // if the function is @disabled, maybe there
    // is an overload in the overload set that isn't
    if (isAliasedDeclaration)
    {
        FuncDeclaration *fd = isFuncDeclaration();
        if (fd)
        {
            for (FuncDeclaration *ovl = fd; ovl; ovl = (FuncDeclaration *)ovl->overnext)
                if (!(ovl->storage_class & STCdisable))
                    return false;
        }
    }
    error(loc, "cannot be used because it is annotated with `@disable`");
    return true;
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
        for (size_t i = 0; i < objects->length; i++)
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
        args->setDim(objects->length);
        OutBuffer buf;
        int hasdeco = 1;
        for (size_t i = 0; i < types->length; i++)
        {
            Type *t = (*types)[i];
            //printf("type = %s\n", t->toChars());
            Parameter *arg = new Parameter(0, t, NULL, NULL, NULL);
            (*args)[i] = arg;
            if (!t->deco)
                hasdeco = 0;
        }

        tupletype = new TypeTuple(args);
        if (hasdeco)
            return typeSemantic(tupletype, Loc(), NULL);
    }

    return tupletype;
}

Dsymbol *TupleDeclaration::toAlias2()
{
    //printf("TupleDeclaration::toAlias2() '%s' objects = %s\n", toChars(), objects->toChars());

    for (size_t i = 0; i < objects->length; i++)
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
    for (size_t i = 0; i < objects->length; i++)
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
            Type *t = typeSemantic(type, loc, _scope);
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
            dsymbolSemantic(_import, NULL);
        }
        if (_scope)
        {
            aliasSemantic(this, _scope);
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

VarDeclaration *VarDeclaration::create(Loc loc, Type *type, Identifier *id, Initializer *init)
{
    return new VarDeclaration(loc, type, id, init);
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

void VarDeclaration::setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion)
{
    //printf("VarDeclaration::setFieldOffset(ad = %s) %s\n", ad->toChars(), toChars());

    if (aliassym)
    {
        // If this variable was really a tuple, set the offsets for the tuple fields
        TupleDeclaration *v2 = aliassym->isTupleDeclaration();
        assert(v2);
        for (size_t i = 0; i < v2->objects->length; i++)
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
    for (size_t i = 0; i < ad->fields.length; i++)
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
    unsigned memalignsize = target.fieldalign(t);   // size of member for alignment purposes

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
        dsymbolSemantic(this, _scope);

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
    return protection.kind == Prot::export_;
}

bool VarDeclaration::isImportedSymbol() const
{
    if (protection.kind == Prot::export_ && !_init &&
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
    if (!nestedrefs.contains(fdthis))
        nestedrefs.push(fdthis);

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
    if (!sc->intypeof && !(sc->flags & SCOPEcompile))
    {
        if (!fdv->closureVars.contains(this))
            fdv->closureVars.push(this);
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
        _init = initializerSemantic(_init, _scope, type, INITinterpret);
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
            if (cd->isCPPclass())
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
    protection = Prot(Prot::public_);
    linkage = LINKc;
    alignment = target.ptrsize;
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

const char *TypeInfoDeclaration::toChars()
{
    //printf("TypeInfoDeclaration::toChars() tinfo = %s\n", tinfo->toChars());
    OutBuffer buf;
    buf.writestring("typeid(");
    buf.writestring(tinfo->toChars());
    buf.writeByte(')');
    return buf.extractChars();
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

