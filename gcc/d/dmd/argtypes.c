
/* Compiler implementation of the D programming language
 * Copyright (C) 2010-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/argtypes.c
 */

#include "root/dsystem.h"
#include "root/checkedint.h"

#include "mars.h"
#include "dsymbol.h"
#include "mtype.h"
#include "scope.h"
#include "init.h"
#include "expression.h"
#include "attrib.h"
#include "declaration.h"
#include "template.h"
#include "id.h"
#include "enum.h"
#include "import.h"
#include "aggregate.h"
#include "hdrgen.h"

/****************************************************
 * This breaks a type down into 'simpler' types that can be passed to a function
 * in registers, and returned in registers.
 * It's highly platform dependent.
 * Params:
 *      t = type to break down
 * Returns:
 *      tuple of types, each element can be passed in a register.
 *      A tuple of zero length means the type cannot be passed/returned in registers.
 */

TypeTuple *toArgTypes(Type *t)
{
    class ToArgTypes : public Visitor
    {
    public:
        TypeTuple *result;

        ToArgTypes()
        {
            result = NULL;
        }

        void visit(Type *)
        {
            // not valid for a parameter
        }

        void visit(TypeError *)
        {
            result = new TypeTuple(Type::terror);
        }

        void visit(TypeBasic *t)
        {
            Type *t1 = NULL;
            Type *t2 = NULL;
            switch (t->ty)
            {
                case Tvoid:
                     return;

                case Tbool:
                case Tint8:
                case Tuns8:
                case Tint16:
                case Tuns16:
                case Tint32:
                case Tuns32:
                case Tfloat32:
                case Tint64:
                case Tuns64:
                case Tint128:
                case Tuns128:
                case Tfloat64:
                case Tfloat80:
                    t1 = t;
                    break;

                case Timaginary32:
                    t1 = Type::tfloat32;
                    break;

                case Timaginary64:
                    t1 = Type::tfloat64;
                    break;

                case Timaginary80:
                    t1 = Type::tfloat80;
                    break;

                case Tcomplex32:
                    if (global.params.is64bit)
                        t1 = Type::tfloat64;
                    else
                    {
                        t1 = Type::tfloat64;
                        t2 = Type::tfloat64;
                    }
                    break;

                case Tcomplex64:
                    t1 = Type::tfloat64;
                    t2 = Type::tfloat64;
                    break;

                case Tcomplex80:
                    t1 = Type::tfloat80;
                    t2 = Type::tfloat80;
                    break;

                case Tchar:
                    t1 = Type::tuns8;
                    break;

                case Twchar:
                    t1 = Type::tuns16;
                    break;

                case Tdchar:
                    t1 = Type::tuns32;
                    break;

                default:
                    assert(0);
            }

            if (t1)
            {
                if (t2)
                    result = new TypeTuple(t1, t2);
                else
                    result = new TypeTuple(t1);
            }
            else
                result = new TypeTuple();
        }

        void visit(TypeVector *t)
        {
            result = new TypeTuple(t);
        }

        void visit(TypeSArray *t)
        {
            if (t->dim)
            {
                /* Should really be done as if it were a struct with dim members
                 * of the array's elements.
                 * I.e. int[2] should be done like struct S { int a; int b; }
                 */
                dinteger_t sz = t->dim->toInteger();
                // T[1] should be passed like T
                if (sz == 1)
                {
                    t->next->accept(this);
                    return;
                }
            }
            result = new TypeTuple();     // pass on the stack for efficiency
        }

        void visit(TypeAArray *)
        {
            result = new TypeTuple(Type::tvoidptr);
        }

        void visit(TypePointer *)
        {
            result = new TypeTuple(Type::tvoidptr);
        }

        /*************************************
         * Convert a floating point type into the equivalent integral type.
         */

        static Type *mergeFloatToInt(Type *t)
        {
            switch (t->ty)
            {
                case Tfloat32:
                case Timaginary32:
                    t = Type::tint32;
                    break;
                case Tfloat64:
                case Timaginary64:
                case Tcomplex32:
                    t = Type::tint64;
                    break;
                default:
                    assert(0);
            }
            return t;
        }

        /*************************************
         * This merges two types into an 8byte type.
         * Params:
         *      t1 = first type (can be null)
         *      t2 = second type (can be null)
         *      offset2 = offset of t2 from start of t1
         * Returns:
         *      type that encompasses both t1 and t2, null if cannot be done
         */

        static Type *argtypemerge(Type *t1, Type *t2, unsigned offset2)
        {
            //printf("argtypemerge(%s, %s, %d)\n", t1 ? t1->toChars() : "", t2 ? t2->toChars() : "", offset2);
            if (!t1)
            {   assert(!t2 || offset2 == 0);
                return t2;
            }
            if (!t2)
                return t1;

            const d_uns64 sz1 = t1->size(Loc());
            const d_uns64 sz2 = t2->size(Loc());
            assert(sz1 != SIZE_INVALID && sz2 != SIZE_INVALID);

            if (t1->ty != t2->ty &&
                (t1->ty == Tfloat80 || t2->ty == Tfloat80))
                return NULL;

            // [float,float] => [cfloat]
            if (t1->ty == Tfloat32 && t2->ty == Tfloat32 && offset2 == 4)
                return Type::tfloat64;

            // Merging floating and non-floating types produces the non-floating type
            if (t1->isfloating())
            {
                if (!t2->isfloating())
                    t1 = mergeFloatToInt(t1);
            }
            else if (t2->isfloating())
                t2 = mergeFloatToInt(t2);

            Type *t;

            // Pick type with larger size
            if (sz1 < sz2)
                t = t2;
            else
                t = t1;

            // If t2 does not lie within t1, need to increase the size of t to enclose both
            assert(sz2 < UINT64_MAX - UINT32_MAX);
            if (offset2 && sz1 < offset2 + sz2)
            {
                switch (offset2 + sz2)
                {
                    case 2:
                        t = Type::tint16;
                        break;
                    case 3:
                    case 4:
                        t = Type::tint32;
                        break;
                    default:
                        t = Type::tint64;
                        break;
                }
            }
            return t;
        }

        void visit(TypeDArray *)
        {
            /* Should be done as if it were:
             * struct S { size_t length; void* ptr; }
             */
            if (global.params.is64bit && !global.params.isLP64)
            {
                // For AMD64 ILP32 ABI, D arrays fit into a single integer register.
                unsigned offset = (unsigned)Type::tsize_t->size(Loc());
                Type *t = argtypemerge(Type::tsize_t, Type::tvoidptr, offset);
                if (t)
                {
                    result = new TypeTuple(t);
                    return;
                }
            }
            result = new TypeTuple(Type::tsize_t, Type::tvoidptr);
        }

        void visit(TypeDelegate *)
        {
            /* Should be done as if it were:
             * struct S { size_t length; void* ptr; }
             */
            if (global.params.is64bit && !global.params.isLP64)
            {
                // For AMD64 ILP32 ABI, delegates fit into a single integer register.
                unsigned offset = (unsigned)Type::tsize_t->size(Loc());
                Type *t = argtypemerge(Type::tsize_t, Type::tvoidptr, offset);
                if (t)
                {
                    result = new TypeTuple(t);
                    return;
                }
            }
            result = new TypeTuple(Type::tvoidptr, Type::tvoidptr);
        }

        void visit(TypeStruct *t)
        {
            //printf("TypeStruct::toArgTypes() %s\n", t->toChars());
            if (!t->sym->isPOD() || t->sym->fields.dim == 0)
            {
            Lmemory:
                //printf("\ttoArgTypes() %s => [ ]\n", t->toChars());
                result = new TypeTuple();         // pass on the stack
                return;
            }
            Type *t1 = NULL;
            Type *t2 = NULL;
            const d_uns64 sz = t->size(Loc());
            assert(sz < 0xFFFFFFFF);
            switch ((unsigned)sz)
            {
                case 1:
                    t1 = Type::tint8;
                    break;
                case 2:
                    t1 = Type::tint16;
                    break;
                case 3:
                    if (!global.params.is64bit)
                        goto Lmemory;
                    /* fall through */
                case 4:
                    t1 = Type::tint32;
                    break;
                case 5:
                case 6:
                case 7:
                    if (!global.params.is64bit)
                        goto Lmemory;
                    /* fall through */
                case 8:
                    t1 = Type::tint64;
                    break;
                case 16:
                    t1 = NULL;                   // could be a TypeVector
                    break;
                case 9:
                case 10:
                case 11:
                case 12:
                case 13:
                case 14:
                case 15:
                    if (!global.params.is64bit)
                        goto Lmemory;
                    t1 = NULL;
                    break;
                default:
                    goto Lmemory;
            }
            if (global.params.is64bit && t->sym->fields.dim)
            {
                t1 = NULL;
                for (size_t i = 0; i < t->sym->fields.dim; i++)
                {
                    VarDeclaration *f = t->sym->fields[i];
                    //printf("  [%d] %s f->type = %s\n", (int)i, f->toChars(), f->type->toChars());

                    TypeTuple *tup = toArgTypes(f->type);
                    if (!tup)
                        goto Lmemory;
                    size_t dim = tup->arguments->dim;
                    Type *ft1 = NULL;
                    Type *ft2 = NULL;
                    switch (dim)
                    {
                        case 2:
                            ft1 = (*tup->arguments)[0]->type;
                            ft2 = (*tup->arguments)[1]->type;
                            break;
                        case 1:
                            if (f->offset < 8)
                                ft1 = (*tup->arguments)[0]->type;
                            else
                                ft2 = (*tup->arguments)[0]->type;
                            break;
                        default:
                            goto Lmemory;
                    }

                    if (f->offset & 7)
                    {
                        // Misaligned fields goto Lmemory
                        unsigned alignsz = f->type->alignsize();
                        if (f->offset & (alignsz - 1))
                            goto Lmemory;

                        // Fields that overlap the 8byte boundary goto Lmemory
                        const d_uns64 fieldsz = f->type->size(Loc());
                        assert(fieldsz != SIZE_INVALID && fieldsz < UINT64_MAX - UINT32_MAX);
                        if (f->offset < 8 && (f->offset + fieldsz) > 8)
                            goto Lmemory;
                    }

                    // First field in 8byte must be at start of 8byte
                    assert(t1 || f->offset == 0);
                    //printf("ft1 = %s\n", ft1 ? ft1->toChars() : "null");
                    //printf("ft2 = %s\n", ft2 ? ft2->toChars() : "null");
                    if (ft1)
                    {
                        t1 = argtypemerge(t1, ft1, f->offset);
                        if (!t1)
                            goto Lmemory;
                    }

                    if (ft2)
                    {
                        unsigned off2 = f->offset;
                        if (ft1)
                            off2 = 8;
                        if (!t2 && off2 != 8)
                            goto Lmemory;
                        assert(t2 || off2 == 8);
                        t2 = argtypemerge(t2, ft2, off2 - 8);
                        if (!t2)
                            goto Lmemory;
                    }
                }

                if (t2)
                {
                    if (t1->isfloating() && t2->isfloating())
                    {
                        if ((t1->ty == Tfloat32 || t1->ty == Tfloat64) &&
                            (t2->ty == Tfloat32 || t2->ty == Tfloat64))
                            ;
                        else
                            goto Lmemory;
                    }
                    else if (t1->isfloating())
                        goto Lmemory;
                    else if (t2->isfloating())
                        goto Lmemory;
                    else
                    {
                    }
                }
            }

            //printf("\ttoArgTypes() %s => [%s,%s]\n", t->toChars(), t1 ? t1->toChars() : "", t2 ? t2->toChars() : "");

            if (t1)
            {
                //if (t1) printf("test1: %s => %s\n", toChars(), t1->toChars());
                if (t2)
                    result = new TypeTuple(t1, t2);
                else
                    result = new TypeTuple(t1);
            }
            else
                goto Lmemory;
        }

        void visit(TypeEnum *t)
        {
            t->toBasetype()->accept(this);
        }

        void visit(TypeClass *)
        {
            result = new TypeTuple(Type::tvoidptr);
        }
    };

    ToArgTypes v;
    t->accept(&v);
    return v.result;
}
