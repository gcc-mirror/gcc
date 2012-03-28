/* PR target/52736 */
/* { dg-do run } */
/* { dg-options "-O1 -msse2" } */
/* { dg-require-effective-target sse2_runtime } */

#include <x86intrin.h>

typedef double D __attribute__((may_alias));
__attribute__((aligned(16))) static const double r[4] = { 1., 5., 1., 3. };

__attribute__((noinline, noclone))
void
foo (int x)
{
  asm volatile ("" : "+g" (x) : : "memory");
  if (x != 3)
    __builtin_abort ();
}

int
main ()
{
  __m128d t = _mm_set1_pd (5.);
  ((D *)(&t))[0] = 1.;
  foo (_mm_movemask_pd (_mm_cmpeq_pd (t, _mm_load_pd (&r[0]))));
  ((D *)(&t))[1] = 3.;
  foo (_mm_movemask_pd (_mm_cmpeq_pd (t, _mm_load_pd (&r[2]))));
  return 0;
}
