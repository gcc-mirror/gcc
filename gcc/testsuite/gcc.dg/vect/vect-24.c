/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 64

__attribute__ ((noinline)) int
main1 ()
{
  int i;
  int ia[N];
  int ib[N]= 
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};
  int ic[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  char ca[N];
  char cb[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  char cc[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  short sa[N];
  short sb[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  short sc[N] =
    {1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0,
     1,1,0,0,1,0,1,0};

  /* Check ints.  */

  for (i = 0; i < N; i++)
    {
      ia[i] = (ib[i] || ic[i]);
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (ia[i] != (ib[i] || ic[i]))
        abort ();
    }

  /* Check chars.  */

  for (i = 0; i < N; i++)
    {
      ca[i] = (cb[i] || cc[i]);
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (ca[i] != (cb[i] || cc[i]))
        abort ();
    }

  /* Check shorts.  */

  for (i = 0; i < N; i++)
    {
      sa[i] = (sb[i] || sc[i]);
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i <N; i++)
    {
      if (sa[i] != (sb[i] || sc[i]))
        abort ();
    }

  return 0;
}

int main (void)
{
  check_vect ();

  return main1 ();
}
/* The short-cutting || is if-converted using COND_EXPRs rather than
   bitwise or.  */
/* { dg-final { scan-tree-dump-times "vectorized 3 loops" 1 "vect" { xfail { ! vect_condition } } } } */
/* { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 0 "vect" { xfail { ! vect_align_stack_vars } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
