/* { dg-do run { target *-*-*gnu* } } */
/* { dg-additional-options "-D_GNU_SOURCE" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>

typedef int v4si __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));

void __attribute__((noipa))
foo (v4si *dstp, v4sf *srcp)
{
  v4sf src = *srcp;
  *dstp = (v4si) { src[0], src[1], 3, 4 };
}

void __attribute__((noipa))
bar (v4sf *dstp, v4si *srcp)
{
  v4si src = *srcp;
  *dstp = (v4sf) { src[0], src[1], 3.5, 4.5 };
}

int
main()
{
  feenableexcept (FE_INVALID|FE_INEXACT);
  v4sf x = (v4sf) { 1, 2, __builtin_nanf (""), 3.5 };
  v4si y;
  foo (&y, &x);
  if (y[0] != 1 || y[1] != 2 || y[2] != 3 || y[3] != 4)
    __builtin_abort ();
  y = (v4si) { 0, 1, __INT_MAX__, -__INT_MAX__ };
  bar (&x, &y);
  if (x[0] != 0 || x[1] != 1 || x[2] != 3.5 || x[3] != 4.5)
    __builtin_abort ();
  return 0;
}
