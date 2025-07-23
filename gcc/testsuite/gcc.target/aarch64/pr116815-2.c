/* { dg-do compile } */
/* { dg-options "-O2" } */

#pragma GCC target "+cssc"

/* PR middle-end/116815 */

/* Make sure that umax/umin instructions are generated with CSSC.  */

static inline unsigned __attribute__ ((always_inline))
max (unsigned a, unsigned b)
{
  return a > b ? a : b;
}

static inline unsigned __attribute__ ((always_inline))
min (unsigned a, unsigned b)
{
  return a < b ? a : b;
}

#define OPERATION(op, type, N, exp1, exp2)                                     \
  unsigned u##op##type##N (unsigned a, unsigned b) { return op (exp1, exp2); }

OPERATION (max, add, 1, a, a + b)
OPERATION (max, add, 2, a, b + a)
OPERATION (max, add, 3, a + b, a)
OPERATION (max, add, 4, b + a, a)

OPERATION (min, add, 1, a, a + b)
OPERATION (min, add, 2, a, b + a)
OPERATION (min, add, 3, a + b, a)
OPERATION (min, add, 4, b + a, a)

OPERATION (max, sub, 1, a, a - b)
OPERATION (max, sub, 2, a - b, a)

OPERATION (min, sub, 1, a, a - b)
OPERATION (min, sub, 2, a - b, a)

/* { dg-final { scan-assembler-times "umax\\t" 6 } } */
/* { dg-final { scan-assembler-times "umin\\t" 6 } } */
/* { dg-final { scan-assembler-not "adds\\t" } } */
/* { dg-final { scan-assembler-not "subs\\t" } } */
