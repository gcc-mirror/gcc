/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

typedef float __attribute__((aligned(8))) float_aligned;

vector float
foo (float_aligned *a)
{
  return vec_xl (0, a);
}

vector float
bar (const float_aligned *a)
{
  return vec_xl (0, a);
}

void
baz (float_aligned *f, vector float a)
{
  vec_xst (a, 0, f);
}

vector float
foo2 (float_aligned *a)
{
  return vec_xlw4 (0, a);
}

vector float
bar2 (const float_aligned *a)
{
  return vec_xlw4 (0, a);
}

void
baz2 (float_aligned *f, vector float a)
{
  vec_xstw4 (a, 0, f);
}

/* Make sure alignment hints are generated if the source or target
   operand is properly aligned.  */

/* { dg-final { scan-assembler-times "vl\t%v\[0-9\]*,0\\(%r2\\),3" 4 } } */
/* { dg-final { scan-assembler-times "vst\t%v\[0-9\]*,0\\(%r2\\),3" 2 } } */
