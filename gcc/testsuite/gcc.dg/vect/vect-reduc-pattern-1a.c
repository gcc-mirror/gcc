/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 2 / 16)
#else
#define N 16
#endif

unsigned short udata_sh[N] =
  { 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28 };
#define SUM 210

__attribute__ ((noinline)) int
foo ()
{
  int i;
  unsigned int intsum = 0;

  /* widenning sum: sum shorts into int.  */
  for (i = 0; i < N; i++)
    {
      intsum += udata_sh[i];
    }

  /* check results:  */
  if (intsum != SUM)
    abort ();

  return 0;
}

int
main (void)
{
  check_vect ();
  return foo ();
}

/* { dg-final { scan-tree-dump-times "vect_recog_widen_sum_pattern: detected" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_widen_sum_hi_to_si } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { ! vect_widen_sum_hi_to_si } } } } */
