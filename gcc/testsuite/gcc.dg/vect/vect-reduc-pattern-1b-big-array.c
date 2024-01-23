/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
unsigned char udata_ch[N];
#define SUM N*(N-1)

__attribute__ ((noinline)) int
foo ()
{
  int i;
  unsigned int intsum = 0;

  for (i = 0; i < N; i++)
    {
      udata_ch[i] = i*2;
      asm volatile ("" ::: "memory");
    }

  /* widenning sum: sum chars into int.  */
  for (i = 0; i < N; i++)
    {
      intsum += udata_ch[i];
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

/* { dg-final { scan-tree-dump-times "vect_recog_widen_sum_pattern: detected(?:(?!failed)(?!Re-trying).)*succeeded" 1 "vect" { target { vect_widen_sum_qi_to_si || vect_unpack } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_sum_qi_to_si || vect_unpack } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { { ! vect_widen_sum_qi_to_si } && { ! vect_unpack } } } } } */
