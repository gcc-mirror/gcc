
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/safe.c
 */

#include "mars.h"
#include "expression.h"
#include "scope.h"
#include "aggregate.h"
#include "target.h"

bool MODimplicitConv(MOD modfrom, MOD modto);

/*************************************************************
 * Check for unsafe access in @safe code:
 * 1. read overlapped pointers
 * 2. write misaligned pointers
 * 3. write overlapped storage classes
 * Print error if unsafe.
 * Params:
 *      sc = scope
 *      e = expression to check
 *      readonly = if access is read-only
 *      printmsg = print error message if true
 * Returns:
 *      true if error
 */

bool checkUnsafeAccess(Scope *sc, Expression *e, bool readonly, bool printmsg)
{
    if (e->op != TOKdotvar)
        return false;
    DotVarExp *dve = (DotVarExp *)e;
    if (VarDeclaration *v = dve->var->isVarDeclaration())
    {
        if (sc->intypeof || !sc->func || !sc->func->isSafeBypassingInference())
            return false;

        AggregateDeclaration *ad = v->toParent2()->isAggregateDeclaration();
        if (!ad)
            return false;

        if (v->overlapped && v->type->hasPointers() && sc->func->setUnsafe())
        {
            if (printmsg)
                e->error("field %s.%s cannot access pointers in @safe code that overlap other fields",
                    ad->toChars(), v->toChars());
            return true;
        }

        if (readonly || !e->type->isMutable())
            return false;

        if (v->type->hasPointers() && v->type->toBasetype()->ty != Tstruct)
        {
            if ((ad->type->alignment() < (unsigned)Target::ptrsize ||
                 (v->offset & (Target::ptrsize - 1))) &&
                sc->func->setUnsafe())
            {
                if (printmsg)
                    e->error("field %s.%s cannot modify misaligned pointers in @safe code",
                        ad->toChars(), v->toChars());
                return true;
            }
        }

        if (v->overlapUnsafe && sc->func->setUnsafe())
        {
            if (printmsg)
                e->error("field %s.%s cannot modify fields in @safe code that overlap fields with other storage classes",
                    ad->toChars(), v->toChars());
            return true;
        }
    }
    return false;
}


/**********************************************
 * Determine if it is @safe to cast e from tfrom to tto.
 * Params:
 *      e = expression to be cast
 *      tfrom = type of e
 *      tto = type to cast e to
 * Returns:
 *      true if @safe
 */
bool isSafeCast(Expression *e, Type *tfrom, Type *tto)
{
    // Implicit conversions are always safe
    if (tfrom->implicitConvTo(tto))
        return true;

    if (!tto->hasPointers())
        return true;

    Type *ttob = tto->toBasetype();

    if (ttob->ty == Tclass && tfrom->ty == Tclass)
    {
        ClassDeclaration *cdfrom = tfrom->isClassHandle();
        ClassDeclaration *cdto = ttob->isClassHandle();

        int offset;
        if (!cdfrom->isBaseOf(cdto, &offset))
            return false;

        if (cdfrom->isCPPinterface() || cdto->isCPPinterface())
            return false;

        if (!MODimplicitConv(tfrom->mod, ttob->mod))
            return false;
        return true;
    }

    if (ttob->ty == Tarray && tfrom->ty == Tsarray) // Bugzilla 12502
        tfrom = tfrom->nextOf()->arrayOf();

    if ((ttob->ty == Tarray   && tfrom->ty == Tarray) ||
        (ttob->ty == Tpointer && tfrom->ty == Tpointer))
    {
        Type *ttobn = ttob->nextOf()->toBasetype();
        Type *tfromn = tfrom->nextOf()->toBasetype();

        /* From void[] to anything mutable is unsafe because:
         *  int*[] api;
         *  void[] av = api;
         *  int[] ai = cast(int[]) av;
         *  ai[0] = 7;
         *  *api[0] crash!
         */
        if (tfromn->ty == Tvoid && ttobn->isMutable())
        {
            if (ttob->ty == Tarray && e->op == TOKarrayliteral)
                return true;
            return false;
        }

        // If the struct is opaque we don't know about the struct members then the cast becomes unsafe
        if ((ttobn->ty == Tstruct && !((TypeStruct *)ttobn)->sym->members) ||
            (tfromn->ty == Tstruct && !((TypeStruct *)tfromn)->sym->members))
            return false;

        const bool frompointers = tfromn->hasPointers();
        const bool topointers = ttobn->hasPointers();

        if (frompointers && !topointers && ttobn->isMutable())
            return false;

        if (!frompointers && topointers)
            return false;

        if (!topointers &&
            ttobn->ty != Tfunction && tfromn->ty != Tfunction &&
            (ttob->ty == Tarray || ttobn->size() <= tfromn->size()) &&
            MODimplicitConv(tfromn->mod, ttobn->mod))
        {
            return true;
        }
    }
    return false;
}

