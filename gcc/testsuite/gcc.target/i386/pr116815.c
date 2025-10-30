/* PR target/116815 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-march=pentiumpro" { target ia32 } } */

static inline __attribute__ ((always_inline))
unsigned max (unsigned a, unsigned b) { return a > b ? a : b; }

static inline __attribute__ ((always_inline))
unsigned min (unsigned a, unsigned b) { return a < b ? a : b; }

#define OPERATION(op, type, N, exp1, exp2) \
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

/* { dg-final { scan-assembler-not "cmp" } } */
