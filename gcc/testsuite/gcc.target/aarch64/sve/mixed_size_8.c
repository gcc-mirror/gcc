/* { dg-options "-O2 -msve-vector-bits=512" } */

typedef int int32x16_t __attribute__((vector_size(64)));
typedef int int32x8_t __attribute__((vector_size(32)));

int32x8_t
f1 (int32x16_t x)
{
  union u { int32x16_t full; int32x8_t pair[2]; } u;
  u.full = x | 2;
  return u.pair[0] + (int32x8_t) { 1, 2, 3, 4, 5, 6, 7, 8 };
}

int32x8_t
f2 (int32x16_t x)
{
  union u { int32x16_t full; int32x8_t pair[2]; } u;
  u.full = x | 2;
  return u.pair[1] + (int32x8_t) { 1, 2, 3, 4, 5, 6, 7, 8 };
}

/* We could do something more efficient than spill the int32x16_t and
   reload the int32x8_t.  The important thing is that we don't do
   something like:

	orr	z0.s, z0.s, #2
	index	z1.d, #1, #1
	add	z0.s, z0.s, z1.s
	st1w	z0.d, p0, [x8]

   We're supposed to add z1 to one half of the ORR result instead.  */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.d} 2 } } */
