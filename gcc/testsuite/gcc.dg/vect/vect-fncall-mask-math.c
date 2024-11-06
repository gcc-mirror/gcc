/* Test the correct application of masking to autovectorized math function calls.
   Test is currently set to xfail pending the release of the relevant lmvec
   support. */
/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-additional-options "-march=armv8.2-a+sve -fdump-tree-ifcvt-raw -Ofast" { target { aarch64*-*-* } } } */

#include <math.h>

const int N = 20;
const float lim = 101.0;
const float cst =  -1.0;
float tot =   0.0;

float b[20];
float a[20] = { [0 ... 9] = 1.7014118e39, /* If branch. */
		[10 ... 19] = 100.0 };    /* Else branch.  */

int main (void)
{
  #pragma omp simd
  for (int i = 0; i < N; i += 1)
    {
      if (a[i] > lim)
	b[i] = cst;
      else
	b[i] = expf (a[i]);
      tot += b[i];
    }
  return (0);
}

/* { dg-final { scan-tree-dump-not { gimple_call <expf, _2, _1>} ifcvt { xfail { aarch64*-*-* } } } } */
/* { dg-final { scan-tree-dump { gimple_call <.MASK_CALL, _2, expf, _1, _30>} ifcvt { xfail { aarch64*-*-* } } } } */
