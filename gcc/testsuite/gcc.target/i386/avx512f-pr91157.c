/* PR tree-optimization/91157 */
/* { dg-do run { target { avx512f && lp64 } } } */
/* { dg-options "-O2 -mavx512f -fexceptions -fnon-call-exceptions -fsignaling-nans" } */

#include "avx512f-helper.h"

typedef long double V __attribute__ ((vector_size (4 * sizeof (long double))));
typedef __int128 W __attribute__ ((vector_size (4 * sizeof (__int128))));

__attribute__((noipa)) W
foo (V x)
{
  return x == 0;
}

static void
test_512 (void)
{
  V a = { 5.0L, 0.0L, -0.0L, -17.0L };
  V b = { -0.0L, 16.0L, 0.0L, 18.0L };
  V c = { 6.0L, 7.0L, 8.0L, 0.0L };
  W ar = foo (a);
  W br = foo (b);
  W cr = foo (c);
  if (ar[0] != 0 || ar[1] != -1 || ar[2] != -1 || ar[3] != 0
      || br[0] != -1 || br[1] != 0 || br[2] != -1 || br[3] != 0
      || cr[0] != 0 || cr[1] != 0 || cr[2] != 0 || cr[3] != -1)
    __builtin_abort ();
}
