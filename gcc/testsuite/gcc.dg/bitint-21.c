/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=gnu23" } */

#define IB __SIZEOF_INT__ * __CHAR_BIT__
typedef _BitInt(IB) V1 __attribute__((vector_size (sizeof (_BitInt(IB)))));		/* { dg-error "invalid vector type for attribute 'vector_size'" } */
typedef _BitInt(IB) V2 __attribute__((vector_size (8 * sizeof (_BitInt(IB)))));		/* { dg-error "invalid vector type for attribute 'vector_size'" } */
#if __BITINT_MAXWIDTH__ >= 575
typedef _BitInt(575) V3 __attribute__((vector_size (sizeof (_BitInt(575)))));		/* { dg-error "invalid vector type for attribute 'vector_size'" "" { target bitint575 } } */
typedef _BitInt(575) V3 __attribute__((vector_size (16 * sizeof (_BitInt(575)))));	/* { dg-error "invalid vector type for attribute 'vector_size'" "" { target bitint575 } } */
#endif
