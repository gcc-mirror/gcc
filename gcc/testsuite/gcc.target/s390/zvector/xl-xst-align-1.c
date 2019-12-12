/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

vector float
foo (float *a)
{
  return vec_xl (0, a);
}

vector float
bar (const float *a)
{
  return vec_xl (0, a);
}

void
baz (float *f, vector float a)
{
  vec_xst (a, 0, f);
}

vector float
foo2 (float *a)
{
  return vec_xlw4 (0, a);
}

vector float
bar2 (const float *a)
{
  return vec_xlw4 (0, a);
}

void
baz2 (float *f, vector float a)
{
  vec_xstw4 (a, 0, f);
}

/* Make sure no alignment hints are generated.  */

/* { dg-final { scan-assembler-not "vl.*,3" } } */
/* { dg-final { scan-assembler-not "vst.*,3" } } */
