/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 128
signed char data_ch[N];

__attribute__ ((noinline)) int
foo ()
{
  int i;
  signed int intsum = 0;
  signed int check_intsum = 0;

  for (i = 0; i < N; i++)
    {
      data_ch[i] = i*2;
      check_intsum += data_ch[i];
      asm volatile ("" ::: "memory");
    }

  /* widenning sum: sum chars into int.  */
  for (i = 0; i < N; i++)
    {
      intsum += data_ch[i];
    }

  /* check results:  */
  if (intsum != check_intsum)
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
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_widen_sum_qi_to_si && vect_unpack } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect" { target { { ! vect_widen_sum_qi_to_si } && { ! vect_unpack } } } } } */
