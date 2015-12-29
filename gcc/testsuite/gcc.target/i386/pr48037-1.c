/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -fno-math-errno" } */

typedef double __m128d __attribute__((vector_size(16)));
__m128d vsqrt1 (__m128d const x)
{
  double const* __restrict__ const y = (double const*)&x;
  double const a = __builtin_sqrt(y[0]);
  double const b = __builtin_sqrt(y[1]);
  return (__m128d) { a, b };
}

/* Verify we do not spill x to the stack.  */
/* { dg-final { scan-assembler-not "%rsp" } } */
