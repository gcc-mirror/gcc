
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/struct.c
 */

#include "root/dsystem.h"
#include "root/root.h"

#include "errors.h"
#include "aggregate.h"
#include "scope.h"
#include "mtype.h"
#include "init.h"
#include "declaration.h"
#include "module.h"
#include "id.h"
#include "statement.h"
#include "template.h"
#include "tokens.h"
#include "target.h"

Type *getTypeInfoType(Loc loc, Type *t, Scope *sc);
void unSpeculative(Scope *sc, RootObject *o);
bool MODimplicitConv(MOD modfrom, MOD modto);
Expression *resolve(Loc loc, Scope *sc, Dsymbol *s, bool hasOverloads);

FuncDeclaration *StructDeclaration::xerreq;     // object.xopEquals
FuncDeclaration *StructDeclaration::xerrcmp;    // object.xopCmp

/***************************************
 * Search toString member function for TypeInfo_Struct.
 *      string toString();
 */
FuncDeclaration *search_toString(StructDeclaration *sd)
{
    Dsymbol *s = search_function(sd, Id::tostring);
    FuncDeclaration *fd = s ? s->isFuncDeclaration() : NULL;
    if (fd)
    {
        static TypeFunction *tftostring;
        if (!tftostring)
        {
            tftostring = new TypeFunction(ParameterList(), Type::tstring, LINKd);
            tftostring = tftostring->merge()->toTypeFunction();
        }

        fd = fd->overloadExactMatch(tftostring);
    }
    return fd;
}

/***************************************
 * Request additonal semantic analysis for TypeInfo generation.
 */
void semanticTypeInfo(Scope *sc, Type *t)
{
    class FullTypeInfoVisitor : public Visitor
    {
    public:
        Scope *sc;

        void visit(Type *t)
        {
            Type *tb = t->toBasetype();
            if (tb != t)
                tb->accept(this);
        }
        void visit(TypeNext *t)
        {
            if (t->next)
                t->next->accept(this);
        }
        void visit(TypeBasic *) { }
        void visit(TypeVector *t)
        {
            t->basetype->accept(this);
        }
        void visit(TypeAArray *t)
        {
            t->index->accept(this);
            visit((TypeNext *)t);
        }
        void visit(TypeFunction *t)
        {
            visit((TypeNext *)t);
            // Currently TypeInfo_Function doesn't store parameter types.
        }
        void visit(TypeStruct *t)
        {
            //printf("semanticTypeInfo::visit(TypeStruct = %s)\n", t->toChars());
            StructDeclaration *sd = t->sym;

            /* Step 1: create TypeInfoDeclaration
             */
            if (!sc) // inline may request TypeInfo.
            {
                Scope scx;
                scx._module = sd->getModule();
                getTypeInfoType(sd->loc, t, &scx);
                sd->requestTypeInfo = true;
            }
            else if (!sc->minst)
            {
                // don't yet have to generate TypeInfo instance if
                // the typeid(T) expression exists in speculative scope.
            }
            else
            {
                getTypeInfoType(sd->loc, t, sc);
                sd->requestTypeInfo = true;

                // Bugzilla 15149, if the typeid operand type comes from a
                // result of auto function, it may be yet speculative.
                // unSpeculative(sc, sd);
            }

            /* Step 2: If the TypeInfo generation requires sd.semantic3, run it later.
             * This should be done even if typeid(T) exists in speculative scope.
             * Because it may appear later in non-speculative scope.
             */
            if (!sd->members)
                return;     // opaque struct
            if (!sd->xeq && !sd->xcmp && !sd->postblit &&
                !sd->dtor && !sd->xhash && !search_toString(sd))
                return;     // none of TypeInfo-specific members

            // If the struct is in a non-root module, run semantic3 to get
            // correct symbols for the member function.
            if (sd->semanticRun >= PASSsemantic3)
            {
                // semantic3 is already done
            }
            else if (TemplateInstance *ti = sd->isInstantiated())
            {
                if (ti->minst && !ti->minst->isRoot())
                    Module::addDeferredSemantic3(sd);
            }
            else
            {
                if (sd->inNonRoot())
                {
                    //printf("deferred sem3 for TypeInfo - sd = %s, inNonRoot = %d\n", sd->toChars(), sd->inNonRoot());
                    Module::addDeferredSemantic3(sd);
                }
            }
        }
        void visit(TypeClass *) { }
        void visit(TypeTuple *t)
        {
            if (t->arguments)
            {
                for (size_t i = 0; i < t->arguments->length; i++)
                {
                    Type *tprm = (*t->arguments)[i]->type;
                    if (tprm)
                        tprm->accept(this);
                }
            }
        }
    };

    if (sc)
    {
        if (!sc->func)
            return;
        if (sc->intypeof)
            return;
        if (sc->flags & (SCOPEctfe | SCOPEcompile))
            return;
    }

    FullTypeInfoVisitor v;
    v.sc = sc;
    t->accept(&v);
}

/********************************* AggregateDeclaration ****************************/

AggregateDeclaration::AggregateDeclaration(Loc loc, Identifier *id)
    : ScopeDsymbol(id)
{
    this->loc = loc;

    storage_class = 0;
    protection = Prot(Prot::public_);
    type = NULL;
    structsize = 0;             // size of struct
    alignsize = 0;              // size of struct for alignment purposes
    sizeok = SIZEOKnone;        // size not determined yet
    deferred = NULL;
    isdeprecated = false;
    classKind = ClassKind::d;
    inv = NULL;
    aggNew = NULL;
    aggDelete = NULL;

    stag = NULL;
    sinit = NULL;
    enclosing = NULL;
    vthis = NULL;

    ctor = NULL;
    defaultCtor = NULL;
    aliasthis = NULL;
    noDefaultCtor = false;
    dtor = NULL;
    getRTInfo = NULL;
}

Prot AggregateDeclaration::prot()
{
    return protection;
}

/***************************************
 * Create a new scope from sc.
 * semantic, semantic2 and semantic3 will use this for aggregate members.
 */
Scope *AggregateDeclaration::newScope(Scope *sc)
{
    Scope *sc2 = sc->push(this);
    sc2->stc &= STCsafe | STCtrusted | STCsystem;
    sc2->parent = this;
    if (isUnionDeclaration())
        sc2->inunion = 1;
    sc2->protection = Prot(Prot::public_);
    sc2->explicitProtection = 0;
    sc2->aligndecl = NULL;
    sc2->userAttribDecl = NULL;
    return sc2;
}

void AggregateDeclaration::setScope(Scope *sc)
{
    // Might need a scope to resolve forward references. The check for
    // semanticRun prevents unnecessary setting of _scope during deferred
    // setScope phases for aggregates which already finished semantic().
    // Also see https://issues.dlang.org/show_bug.cgi?id=16607
    if (semanticRun < PASSsemanticdone)
        ScopeDsymbol::setScope(sc);
}

void AggregateDeclaration::semantic2(Scope *sc)
{
    //printf("AggregateDeclaration::semantic2(%s) type = %s, errors = %d\n", toChars(), type->toChars(), errors);
    if (!members)
        return;

    if (_scope)
    {
        error("has forward references");
        return;
    }

    Scope *sc2 = newScope(sc);

    determineSize(loc);

    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        //printf("\t[%d] %s\n", i, s->toChars());
        s->semantic2(sc2);
    }

    sc2->pop();
}

void AggregateDeclaration::semantic3(Scope *sc)
{
    //printf("AggregateDeclaration::semantic3(%s) type = %s, errors = %d\n", toChars(), type->toChars(), errors);
    if (!members)
        return;

    StructDeclaration *sd = isStructDeclaration();
    if (!sc)    // from runDeferredSemantic3 for TypeInfo generation
    {
        assert(sd);
        sd->semanticTypeInfoMembers();
        return;
    }

    Scope *sc2 = newScope(sc);

    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->semantic3(sc2);
    }

    sc2->pop();

    // don't do it for unused deprecated types
    // or error types
    if (!getRTInfo && Type::rtinfo &&
        (!isDeprecated() || global.params.useDeprecated != DIAGNOSTICerror) &&
        (type && type->ty != Terror))
    {
        // Evaluate: RTinfo!type
        Objects *tiargs = new Objects();
        tiargs->push(type);
        TemplateInstance *ti = new TemplateInstance(loc, Type::rtinfo, tiargs);

        Scope *sc3 = ti->tempdecl->_scope->startCTFE();
        sc3->tinst = sc->tinst;
        sc3->minst = sc->minst;
        if (isDeprecated())
            sc3->stc |= STCdeprecated;

        ti->semantic(sc3);
        ti->semantic2(sc3);
        ti->semantic3(sc3);
        Expression *e = resolve(Loc(), sc3, ti->toAlias(), false);

        sc3->endCTFE();

        e = e->ctfeInterpret();
        getRTInfo = e;
    }

    if (sd)
        sd->semanticTypeInfoMembers();
    semanticRun = PASSsemantic3done;
}

/***************************************
 * Find all instance fields, then push them into `fields`.
 *
 * Runs semantic() for all instance field variables, but also
 * the field types can reamin yet not resolved forward references,
 * except direct recursive definitions.
 * After the process sizeok is set to SIZEOKfwd.
 *
 * Returns:
 *      false if any errors occur.
 */
bool AggregateDeclaration::determineFields()
{
    if (sizeok != SIZEOKnone)
        return true;

    //printf("determineFields() %s, fields.length = %d\n", toChars(), fields.length);
    fields.setDim(0);

    struct SV
    {
        AggregateDeclaration *agg;

        static int func(Dsymbol *s, void *param)
        {
            VarDeclaration *v = s->isVarDeclaration();
            if (!v)
                return 0;
            if (v->storage_class & STCmanifest)
                return 0;

            AggregateDeclaration *ad = ((SV *)param)->agg;

            if (v->semanticRun < PASSsemanticdone)
                v->semantic(NULL);
            // Note: Aggregate fields or size could have determined during v->semantic.
            if (ad->sizeok != SIZEOKnone)
                return 1;

            if (v->aliassym)
                return 0;   // If this variable was really a tuple, skip it.

            if (v->storage_class & (STCstatic | STCextern | STCtls | STCgshared | STCmanifest | STCctfe | STCtemplateparameter))
                return 0;
            if (!v->isField() || v->semanticRun < PASSsemanticdone)
                return 1;   // unresolvable forward reference

            ad->fields.push(v);

            if (v->storage_class & STCref)
                return 0;
            Type *tv = v->type->baseElemOf();
            if (tv->ty != Tstruct)
                return 0;
            if (ad == ((TypeStruct *)tv)->sym)
            {
                const char *psz = (v->type->toBasetype()->ty == Tsarray) ? "static array of " : "";
                ad->error("cannot have field %s with %ssame struct type", v->toChars(), psz);
                ad->type = Type::terror;
                ad->errors = true;
                return 1;
            }
            return 0;
        }
    };
    SV sv;
    sv.agg = this;

    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        if (s->apply(&SV::func, &sv))
        {
            if (sizeok != SIZEOKnone)
                return true;
            return false;
        }
    }

    if (sizeok != SIZEOKdone)
        sizeok = SIZEOKfwd;

    return true;
}

/***************************************
 * Collect all instance fields, then determine instance size.
 * Returns:
 *      false if failed to determine the size.
 */
bool AggregateDeclaration::determineSize(Loc loc)
{
    //printf("AggregateDeclaration::determineSize() %s, sizeok = %d\n", toChars(), sizeok);

    // The previous instance size finalizing had:
    if (type->ty == Terror)
        return false;   // failed already
    if (sizeok == SIZEOKdone)
        return true;    // succeeded

    if (!members)
    {
        error(loc, "unknown size");
        return false;
    }

    if (_scope)
        semantic(NULL);

    // Determine the instance size of base class first.
    if (ClassDeclaration *cd = isClassDeclaration())
    {
        cd = cd->baseClass;
        if (cd && !cd->determineSize(loc))
            goto Lfail;
    }

    // Determine instance fields when sizeok == SIZEOKnone
    if (!determineFields())
        goto Lfail;
    if (sizeok != SIZEOKdone)
        finalizeSize();

    // this aggregate type has:
    if (type->ty == Terror)
        return false;   // marked as invalid during the finalizing.
    if (sizeok == SIZEOKdone)
        return true;    // succeeded to calculate instance size.

Lfail:
    // There's unresolvable forward reference.
    if (type != Type::terror)
        error(loc, "no size because of forward reference");
    // Don't cache errors from speculative semantic, might be resolvable later.
    // https://issues.dlang.org/show_bug.cgi?id=16574
    if (!global.gag)
    {
        type = Type::terror;
        errors = true;
    }
    return false;
}

void StructDeclaration::semanticTypeInfoMembers()
{
    if (xeq &&
        xeq->_scope &&
        xeq->semanticRun < PASSsemantic3done)
    {
        unsigned errors = global.startGagging();
        xeq->semantic3(xeq->_scope);
        if (global.endGagging(errors))
            xeq = xerreq;
    }

    if (xcmp &&
        xcmp->_scope &&
        xcmp->semanticRun < PASSsemantic3done)
    {
        unsigned errors = global.startGagging();
        xcmp->semantic3(xcmp->_scope);
        if (global.endGagging(errors))
            xcmp = xerrcmp;
    }

    FuncDeclaration *ftostr = search_toString(this);
    if (ftostr &&
        ftostr->_scope &&
        ftostr->semanticRun < PASSsemantic3done)
    {
        ftostr->semantic3(ftostr->_scope);
    }

    if (xhash &&
        xhash->_scope &&
        xhash->semanticRun < PASSsemantic3done)
    {
        xhash->semantic3(xhash->_scope);
    }

    if (postblit &&
        postblit->_scope &&
        postblit->semanticRun < PASSsemantic3done)
    {
        postblit->semantic3(postblit->_scope);
    }

    if (dtor &&
        dtor->_scope &&
        dtor->semanticRun < PASSsemantic3done)
    {
        dtor->semantic3(dtor->_scope);
    }
}

d_uns64 AggregateDeclaration::size(Loc loc)
{
    //printf("+AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
    bool ok = determineSize(loc);
    //printf("-AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
    return ok ? structsize : SIZE_INVALID;
}

Type *AggregateDeclaration::getType()
{
    return type;
}

bool AggregateDeclaration::isDeprecated()
{
    return isdeprecated;
}

bool AggregateDeclaration::isExport() const
{
    return protection.kind == Prot::export_;
}

/***************************************
 * Calculate field[i].overlapped and overlapUnsafe, and check that all of explicit
 * field initializers have unique memory space on instance.
 * Returns:
 *      true if any errors happen.
 */

bool AggregateDeclaration::checkOverlappedFields()
{
    //printf("AggregateDeclaration::checkOverlappedFields() %s\n", toChars());
    assert(sizeok == SIZEOKdone);
    size_t nfields = fields.length;
    if (isNested())
    {
        ClassDeclaration *cd = isClassDeclaration();
        if (!cd || !cd->baseClass || !cd->baseClass->isNested())
            nfields--;
    }
    bool errors = false;

    // Fill in missing any elements with default initializers
    for (size_t i = 0; i < nfields; i++)
    {
        VarDeclaration *vd = fields[i];
        if (vd->errors)
        {
            errors = true;
            continue;
        }

        VarDeclaration *vx = vd;
        if (vd->_init && vd->_init->isVoidInitializer())
            vx = NULL;

        // Find overlapped fields with the hole [vd->offset .. vd->offset->size()].
        for (size_t j = 0; j < nfields; j++)
        {
            if (i == j)
                continue;
            VarDeclaration *v2 = fields[j];
            if (v2->errors)
            {
                errors = true;
                continue;
            }
            if (!vd->isOverlappedWith(v2))
                continue;

            // vd and v2 are overlapping.
            vd->overlapped = true;
            v2->overlapped = true;

            if (!MODimplicitConv(vd->type->mod, v2->type->mod))
                v2->overlapUnsafe = true;
            if (!MODimplicitConv(v2->type->mod, vd->type->mod))
                vd->overlapUnsafe = true;

            if (!vx)
                continue;
            if (v2->_init && v2->_init->isVoidInitializer())
                continue;

            if (vx->_init && v2->_init)
            {
                ::error(loc, "overlapping default initialization for field %s and %s", v2->toChars(), vd->toChars());
                errors = true;
            }
        }
    }
    return errors;
}

/***************************************
 * Fill out remainder of elements[] with default initializers for fields[].
 * Input:
 *      loc:        location
 *      elements:   explicit arguments which given to construct object.
 *      ctorinit:   true if the elements will be used for default initialization.
 * Returns:
 *      false if any errors occur.
 *      Otherwise, returns true and the missing arguments will be pushed in elements[].
 */
bool AggregateDeclaration::fill(Loc loc, Expressions *elements, bool ctorinit)
{
    //printf("AggregateDeclaration::fill() %s\n", toChars());
    assert(sizeok == SIZEOKdone);
    assert(elements);
    size_t nfields = fields.length - isNested();
    bool errors = false;

    size_t dim = elements->length;
    elements->setDim(nfields);
    for (size_t i = dim; i < nfields; i++)
        (*elements)[i] = NULL;

    // Fill in missing any elements with default initializers
    for (size_t i = 0; i < nfields; i++)
    {
        if ((*elements)[i])
            continue;

        VarDeclaration *vd = fields[i];
        VarDeclaration *vx = vd;
        if (vd->_init && vd->_init->isVoidInitializer())
            vx = NULL;

        // Find overlapped fields with the hole [vd->offset .. vd->offset->size()].
        size_t fieldi = i;
        for (size_t j = 0; j < nfields; j++)
        {
            if (i == j)
                continue;
            VarDeclaration *v2 = fields[j];
            if (!vd->isOverlappedWith(v2))
                continue;

            if ((*elements)[j])
            {
                vx = NULL;
                break;
            }
            if (v2->_init && v2->_init->isVoidInitializer())
                continue;

            if (1)
            {
                /* Prefer first found non-void-initialized field
                 * union U { int a; int b = 2; }
                 * U u;    // Error: overlapping initialization for field a and b
                 */
                if (!vx)
                {
                    vx = v2;
                    fieldi = j;
                }
                else if (v2->_init)
                {
                    ::error(loc, "overlapping initialization for field %s and %s",
                        v2->toChars(), vd->toChars());
                    errors = true;
                }
            }
            else
            {
                // Will fix Bugzilla 1432 by enabling this path always

                /* Prefer explicitly initialized field
                 * union U { int a; int b = 2; }
                 * U u;    // OK (u.b == 2)
                 */
                if (!vx || (!vx->_init && v2->_init))
                {
                    vx = v2;
                    fieldi = j;
                }
                else if (vx != vd && !vx->isOverlappedWith(v2))
                {
                    // Both vx and v2 fills vd, but vx and v2 does not overlap
                }
                else if (vx->_init && v2->_init)
                {
                    ::error(loc, "overlapping default initialization for field %s and %s",
                        v2->toChars(), vd->toChars());
                    errors = true;
                }
                else
                    assert(vx->_init || (!vx->_init && !v2->_init));
            }
        }
        if (vx)
        {
            Expression *e;
            if (vx->type->size() == 0)
            {
                e = NULL;
            }
            else if (vx->_init)
            {
                assert(!vx->_init->isVoidInitializer());
                if (vx->inuse)   // https://issues.dlang.org/show_bug.cgi?id=18057
                {
                    vx->error(loc, "recursive initialization of field");
                    errors = true;
                    e = NULL;
                }
                else
                    e = vx->getConstInitializer(false);
            }
            else
            {
                if ((vx->storage_class & STCnodefaultctor) && !ctorinit)
                {
                    ::error(loc, "field %s.%s must be initialized because it has no default constructor",
                            type->toChars(), vx->toChars());
                    errors = true;
                }

                /* Bugzilla 12509: Get the element of static array type.
                 */
                Type *telem = vx->type;
                if (telem->ty == Tsarray)
                {
                    /* We cannot use Type::baseElemOf() here.
                     * If the bottom of the Tsarray is an enum type, baseElemOf()
                     * will return the base of the enum, and its default initializer
                     * would be different from the enum's.
                     */
                    while (telem->toBasetype()->ty == Tsarray)
                        telem = ((TypeSArray *)telem->toBasetype())->next;

                    if (telem->ty == Tvoid)
                        telem = Type::tuns8->addMod(telem->mod);
                }
                if (telem->needsNested() && ctorinit)
                    e = telem->defaultInit(loc);
                else
                    e = telem->defaultInitLiteral(loc);
            }
            (*elements)[fieldi] = e;
        }
    }

    for (size_t i = 0; i < elements->length; i++)
    {
        Expression *e = (*elements)[i];
        if (e && e->op == TOKerror)
            return false;
    }

    return !errors;
}

/****************************
 * Do byte or word alignment as necessary.
 * Align sizes of 0, as we may not know array sizes yet.
 *
 * alignment: struct alignment that is in effect
 * size: alignment requirement of field
 */

void AggregateDeclaration::alignmember(
        structalign_t alignment,
        unsigned size,
        unsigned *poffset)
{
    //printf("alignment = %d, size = %d, offset = %d\n",alignment,size,offset);
    switch (alignment)
    {
        case (structalign_t) 1:
            // No alignment
            break;

        case (structalign_t) STRUCTALIGN_DEFAULT:
            // Alignment in target.fieldalignsize must match what the
            // corresponding C compiler's default alignment behavior is.
            assert(size > 0 && !(size & (size - 1)));
            *poffset = (*poffset + size - 1) & ~(size - 1);
            break;

        default:
            // Align on alignment boundary, which must be a positive power of 2
            assert(alignment > 0 && !(alignment & (alignment - 1)));
            *poffset = (*poffset + alignment - 1) & ~(alignment - 1);
            break;
    }
}

/****************************************
 * Place a member (mem) into an aggregate (agg), which can be a struct, union or class
 * Returns:
 *      offset to place field at
 *
 * nextoffset:    next location in aggregate
 * memsize:       size of member
 * memalignsize:  natural alignment of member
 * alignment:     alignment in effect for this member
 * paggsize:      size of aggregate (updated)
 * paggalignsize: alignment of aggregate (updated)
 * isunion:       the aggregate is a union
 */
unsigned AggregateDeclaration::placeField(
        unsigned *nextoffset,
        unsigned memsize,
        unsigned memalignsize,
        structalign_t alignment,
        unsigned *paggsize,
        unsigned *paggalignsize,
        bool isunion
        )
{
    unsigned ofs = *nextoffset;

    const unsigned actualAlignment =
        alignment == STRUCTALIGN_DEFAULT ? memalignsize : alignment;

    alignmember(alignment, memalignsize, &ofs);
    unsigned memoffset = ofs;
    ofs += memsize;
    if (ofs > *paggsize)
        *paggsize = ofs;
    if (!isunion)
        *nextoffset = ofs;

    if (*paggalignsize < actualAlignment)
        *paggalignsize = actualAlignment;

    return memoffset;
}


/****************************************
 * Returns true if there's an extra member which is the 'this'
 * pointer to the enclosing context (enclosing aggregate or function)
 */

bool AggregateDeclaration::isNested()
{
    return enclosing != NULL;
}

/* Append vthis field (this->tupleof[$-1]) to make this aggregate type nested.
 */
void AggregateDeclaration::makeNested()
{
    if (enclosing)  // if already nested
        return;
    if (sizeok == SIZEOKdone)
        return;
    if (isUnionDeclaration() || isInterfaceDeclaration())
        return;
    if (storage_class & STCstatic)
        return;

    // If nested struct, add in hidden 'this' pointer to outer scope
    Dsymbol *s = toParent2();
    if (!s)
        return;
    Type *t = NULL;
    if (FuncDeclaration *fd = s->isFuncDeclaration())
    {
        enclosing = fd;

        /* Bugzilla 14422: If a nested class parent is a function, its
         * context pointer (== `outer`) should be void* always.
         */
        t = Type::tvoidptr;
    }
    else if (AggregateDeclaration *ad = s->isAggregateDeclaration())
    {
        if (isClassDeclaration() && ad->isClassDeclaration())
        {
            enclosing = ad;
        }
        else if (isStructDeclaration())
        {
            if (TemplateInstance *ti = ad->parent->isTemplateInstance())
            {
                enclosing = ti->enclosing;
            }
        }

        t = ad->handleType();
    }
    if (enclosing)
    {
        //printf("makeNested %s, enclosing = %s\n", toChars(), enclosing->toChars());
        assert(t);
        if (t->ty == Tstruct)
            t = Type::tvoidptr;     // t should not be a ref type
        assert(!vthis);
        vthis = new ThisDeclaration(loc, t);
        //vthis->storage_class |= STCref;

        // Emulate vthis->addMember()
        members->push(vthis);

        // Emulate vthis->semantic()
        vthis->storage_class |= STCfield;
        vthis->parent = this;
        vthis->protection = Prot(Prot::public_);
        vthis->alignment = t->alignment();
        vthis->semanticRun = PASSsemanticdone;

        if (sizeok == SIZEOKfwd)
            fields.push(vthis);
    }
}

/*******************************************
 * Look for constructor declaration.
 */
Dsymbol *AggregateDeclaration::searchCtor()
{
    Dsymbol *s = search(Loc(), Id::ctor);
    if (s)
    {
        if (!(s->isCtorDeclaration() ||
              s->isTemplateDeclaration() ||
              s->isOverloadSet()))
        {
            s->error("is not a constructor; identifiers starting with __ are reserved for the implementation");
            errors = true;
            s = NULL;
        }
    }
    if (s && s->toParent() != this)
        s = NULL; // search() looks through ancestor classes
    if (s)
    {
        // Finish all constructors semantics to determine this->noDefaultCtor.
        struct SearchCtor
        {
            static int fp(Dsymbol *s, void *)
            {
                CtorDeclaration *f = s->isCtorDeclaration();
                if (f && f->semanticRun == PASSinit)
                    f->semantic(NULL);
                return 0;
            }
        };

        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *sm = (*members)[i];
            sm->apply(&SearchCtor::fp, NULL);
        }
    }
    return s;
}

/********************************* StructDeclaration ****************************/

StructDeclaration::StructDeclaration(Loc loc, Identifier *id, bool inObject)
    : AggregateDeclaration(loc, id)
{
    zeroInit = 0;       // assume false until we do semantic processing
    hasIdentityAssign = false;
    hasIdentityEquals = false;
    postblit = NULL;

    xeq = NULL;
    xcmp = NULL;
    xhash = NULL;
    alignment = 0;
    ispod = ISPODfwd;
    arg1type = NULL;
    arg2type = NULL;
    requestTypeInfo = false;

    // For forward references
    type = new TypeStruct(this);

    if (inObject)
    {
        if (id == Id::ModuleInfo && !Module::moduleinfo)
            Module::moduleinfo = this;
    }
}

StructDeclaration *StructDeclaration::create(Loc loc, Identifier *id, bool inObject)
{
    return new StructDeclaration(loc, id, inObject);
}

Dsymbol *StructDeclaration::syntaxCopy(Dsymbol *s)
{
    StructDeclaration *sd =
        s ? (StructDeclaration *)s
          : new StructDeclaration(loc, ident, false);
    return ScopeDsymbol::syntaxCopy(sd);
}

void StructDeclaration::semantic(Scope *sc)
{
    //printf("StructDeclaration::semantic(this=%p, %s '%s', sizeok = %d)\n", this, parent->toChars(), toChars(), sizeok);

    //static int count; if (++count == 20) halt();

    if (semanticRun >= PASSsemanticdone)
        return;
    unsigned errors = global.errors;

    //printf("+StructDeclaration::semantic(this=%p, %s '%s', sizeok = %d)\n", this, parent->toChars(), toChars(), sizeok);
    Scope *scx = NULL;
    if (_scope)
    {
        sc = _scope;
        scx = _scope;            // save so we don't make redundant copies
        _scope = NULL;
    }

    if (!parent)
    {
        assert(sc->parent && sc->func);
        parent = sc->parent;
    }
    assert(parent && !isAnonymous());

    if (this->errors)
        type = Type::terror;
    if (semanticRun == PASSinit)
        type = type->addSTC(sc->stc | storage_class);
    type = type->semantic(loc, sc);

    if (type->ty == Tstruct && ((TypeStruct *)type)->sym != this)
    {
        TemplateInstance *ti = ((TypeStruct *)type)->sym->isInstantiated();
        if (ti && isError(ti))
            ((TypeStruct *)type)->sym = this;
    }

    // Ungag errors when not speculative
    Ungag ungag = ungagSpeculative();

    if (semanticRun == PASSinit)
    {
        protection = sc->protection;

        alignment = sc->alignment();

        storage_class |= sc->stc;
        if (storage_class & STCdeprecated)
            isdeprecated = true;
        if (storage_class & STCabstract)
            error("structs, unions cannot be abstract");
        userAttribDecl = sc->userAttribDecl;

        if (sc->linkage == LINKcpp)
            classKind = ClassKind::cpp;
    }
    else if (symtab && !scx)
    {
        return;
    }
    semanticRun = PASSsemantic;

    if (!members)               // if opaque declaration
    {
        semanticRun = PASSsemanticdone;
        return;
    }
    if (!symtab)
    {
        symtab = new DsymbolTable();

        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf("adding member '%s' to '%s'\n", s->toChars(), this->toChars());
            s->addMember(sc, this);
        }
    }

    Scope *sc2 = newScope(sc);

    /* Set scope so if there are forward references, we still might be able to
     * resolve individual members like enums.
     */
    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        //printf("struct: setScope %s %s\n", s->kind(), s->toChars());
        s->setScope(sc2);
    }

    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->importAll(sc2);
    }

    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->semantic(sc2);
    }

    if (!determineFields())
    {
        assert(type->ty == Terror);
        sc2->pop();
        semanticRun = PASSsemanticdone;
        return;
    }

    /* Following special member functions creation needs semantic analysis
     * completion of sub-structs in each field types. For example, buildDtor
     * needs to check existence of elaborate dtor in type of each fields.
     * See the case in compilable/test14838.d
     */
    for (size_t i = 0; i < fields.length; i++)
    {
        VarDeclaration *v = fields[i];
        Type *tb = v->type->baseElemOf();
        if (tb->ty != Tstruct)
            continue;
        StructDeclaration *sd = ((TypeStruct *)tb)->sym;
        if (sd->semanticRun >= PASSsemanticdone)
            continue;

        sc2->pop();

        _scope = scx ? scx : sc->copy();
        _scope->setNoFree();
        _scope->_module->addDeferredSemantic(this);

        //printf("\tdeferring %s\n", toChars());
        return;
    }

    /* Look for special member functions.
     */
    aggNew =       (NewDeclaration *)search(Loc(), Id::classNew);
    aggDelete = (DeleteDeclaration *)search(Loc(), Id::classDelete);

    // Look for the constructor
    ctor = searchCtor();

    dtor = buildDtor(this, sc2);
    postblit = buildPostBlit(this, sc2);

    buildOpAssign(this, sc2);
    buildOpEquals(this, sc2);

    if (global.params.useTypeInfo && Type::dtypeinfo)  // these functions are used for TypeInfo
    {
        xeq = buildXopEquals(this, sc2);
        xcmp = buildXopCmp(this, sc2);
        xhash = buildXtoHash(this, sc2);
    }

    inv = buildInv(this, sc2);

    Module::dprogress++;
    semanticRun = PASSsemanticdone;
    //printf("-StructDeclaration::semantic(this=%p, '%s')\n", this, toChars());

    sc2->pop();

    if (ctor)
    {
        Dsymbol *scall = search(Loc(), Id::call);
        if (scall)
        {
            unsigned xerrors = global.startGagging();
            sc = sc->push();
            sc->tinst = NULL;
            sc->minst = NULL;
            FuncDeclaration *fcall = resolveFuncCall(loc, sc, scall, NULL, NULL, NULL, 1);
            sc = sc->pop();
            global.endGagging(xerrors);

            if (fcall && fcall->isStatic())
            {
                error(fcall->loc, "static opCall is hidden by constructors and can never be called");
                errorSupplemental(fcall->loc, "Please use a factory method instead, or replace all constructors with static opCall.");
            }
        }
    }

    if (global.errors != errors)
    {
        // The type is no good.
        type = Type::terror;
        this->errors = true;
        if (deferred)
            deferred->errors = true;
    }

    if (deferred && !global.gag)
    {
        deferred->semantic2(sc);
        deferred->semantic3(sc);
    }

    assert(type->ty != Tstruct || ((TypeStruct *)type)->sym == this);
}

Dsymbol *StructDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    //printf("%s.StructDeclaration::search('%s', flags = x%x)\n", toChars(), ident->toChars(), flags);

    if (_scope && !symtab)
        semantic(_scope);

    if (!members || !symtab)    // opaque or semantic() is not yet called
    {
        error("is forward referenced when looking for '%s'", ident->toChars());
        return NULL;
    }

    return ScopeDsymbol::search(loc, ident, flags);
}

void StructDeclaration::finalizeSize()
{
    //printf("StructDeclaration::finalizeSize() %s, sizeok = %d\n", toChars(), sizeok);
    assert(sizeok != SIZEOKdone);

    //printf("+StructDeclaration::finalizeSize() %s, fields.length = %d, sizeok = %d\n", toChars(), fields.length, sizeok);

    fields.setDim(0);   // workaround

    // Set the offsets of the fields and determine the size of the struct
    unsigned offset = 0;
    bool isunion = isUnionDeclaration() != NULL;
    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->setFieldOffset(this, &offset, isunion);
    }
    if (type->ty == Terror)
        return;

    // 0 sized struct's are set to 1 byte
    if (structsize == 0)
    {
        structsize = 1;
        alignsize = 1;
    }

    // Round struct size up to next alignsize boundary.
    // This will ensure that arrays of structs will get their internals
    // aligned properly.
    if (alignment == STRUCTALIGN_DEFAULT)
        structsize = (structsize + alignsize - 1) & ~(alignsize - 1);
    else
        structsize = (structsize + alignment - 1) & ~(alignment - 1);

    sizeok = SIZEOKdone;

    //printf("-StructDeclaration::finalizeSize() %s, fields.length = %d, structsize = %d\n", toChars(), fields.length, structsize);

    if (errors)
        return;

    // Calculate fields[i]->overlapped
    if (checkOverlappedFields())
    {
        errors = true;
        return;
    }

    // Determine if struct is all zeros or not
    zeroInit = 1;
    for (size_t i = 0; i < fields.length; i++)
    {
        VarDeclaration *vd = fields[i];
        if (vd->_init)
        {
            // Should examine init to see if it is really all 0's
            zeroInit = 0;
            break;
        }
        else if (!vd->type->isZeroInit(loc))
        {
            zeroInit = 0;
            break;
        }
    }

    TypeTuple *tt = target.toArgTypes(type);
    size_t dim = tt ? tt->arguments->length : 0;
    if (dim >= 1)
    {
        assert(dim <= 2);
        arg1type = (*tt->arguments)[0]->type;
        if (dim == 2)
            arg2type = (*tt->arguments)[1]->type;
    }
}

/***************************************
 * Fit elements[] to the corresponding type of field[].
 * Input:
 *      loc
 *      sc
 *      elements    The explicit arguments that given to construct object.
 *      stype       The constructed object type.
 * Returns false if any errors occur.
 * Otherwise, returns true and elements[] are rewritten for the output.
 */
bool StructDeclaration::fit(Loc loc, Scope *sc, Expressions *elements, Type *stype)
{
    if (!elements)
        return true;

    size_t nfields = fields.length - isNested();
    size_t offset = 0;
    for (size_t i = 0; i < elements->length; i++)
    {
        Expression *e = (*elements)[i];
        if (!e)
            continue;

        e = resolveProperties(sc, e);
        if (i >= nfields)
        {
            if (i == fields.length - 1 && isNested() && e->op == TOKnull)
            {
                // CTFE sometimes creates null as hidden pointer; we'll allow this.
                continue;
            }
            ::error(loc, "more initializers than fields (%d) of %s", (int)nfields, toChars());
            return false;
        }
        VarDeclaration *v = fields[i];
        if (v->offset < offset)
        {
            ::error(loc, "overlapping initialization for %s", v->toChars());
            return false;
        }
        offset = (unsigned)(v->offset + v->type->size());

        Type *t = v->type;
        if (stype)
            t = t->addMod(stype->mod);
        Type *origType = t;
        Type *tb = t->toBasetype();

        /* Look for case of initializing a static array with a too-short
         * string literal, such as:
         *  char[5] foo = "abc";
         * Allow this by doing an explicit cast, which will lengthen the string
         * literal.
         */
        if (e->op == TOKstring && tb->ty == Tsarray)
        {
            StringExp *se = (StringExp *)e;
            Type *typeb = se->type->toBasetype();
            TY tynto = tb->nextOf()->ty;
            if (!se->committed &&
                (typeb->ty == Tarray || typeb->ty == Tsarray) &&
                (tynto == Tchar || tynto == Twchar || tynto == Tdchar) &&
                se->numberOfCodeUnits(tynto) < ((TypeSArray *)tb)->dim->toInteger())
            {
                e = se->castTo(sc, t);
                goto L1;
            }
        }

        while (!e->implicitConvTo(t) && tb->ty == Tsarray)
        {
            /* Static array initialization, as in:
             *  T[3][5] = e;
             */
            t = tb->nextOf();
            tb = t->toBasetype();
        }
        if (!e->implicitConvTo(t))
            t = origType;  // restore type for better diagnostic

        e = e->implicitCastTo(sc, t);
    L1:
        if (e->op == TOKerror)
            return false;

        (*elements)[i] = doCopyOrMove(sc, e);
    }
    return true;
}

/***************************************
 * Return true if struct is POD (Plain Old Data).
 * This is defined as:
 *      not nested
 *      no postblits, destructors, or assignment operators
 *      no 'ref' fields or fields that are themselves non-POD
 * The idea being these are compatible with C structs.
 */
bool StructDeclaration::isPOD()
{
    // If we've already determined whether this struct is POD.
    if (ispod != ISPODfwd)
        return (ispod == ISPODyes);

    ispod = ISPODyes;

    if (enclosing || postblit || dtor)
        ispod = ISPODno;

    // Recursively check all fields are POD.
    for (size_t i = 0; i < fields.length; i++)
    {
        VarDeclaration *v = fields[i];
        if (v->storage_class & STCref)
        {
            ispod = ISPODno;
            break;
        }

        Type *tv = v->type->baseElemOf();
        if (tv->ty == Tstruct)
        {
            TypeStruct *ts = (TypeStruct *)tv;
            StructDeclaration *sd = ts->sym;
            if (!sd->isPOD())
            {
                ispod = ISPODno;
                break;
            }
        }
    }

    return (ispod == ISPODyes);
}

const char *StructDeclaration::kind() const
{
    return "struct";
}

/********************************* UnionDeclaration ****************************/

UnionDeclaration::UnionDeclaration(Loc loc, Identifier *id)
    : StructDeclaration(loc, id, false)
{
}

Dsymbol *UnionDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    UnionDeclaration *ud = new UnionDeclaration(loc, ident);
    return StructDeclaration::syntaxCopy(ud);
}

const char *UnionDeclaration::kind() const
{
    return "union";
}
