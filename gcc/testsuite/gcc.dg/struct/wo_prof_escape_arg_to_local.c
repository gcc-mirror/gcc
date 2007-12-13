/* { dg-options "-O3 -fno-inline -fipa-type-escape -fdump-ipa-all -fipa-struct-reorg -fwhole-program -combine" } */
/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
struct str
{
  int a;
  float b;
};

#define N 1000

int
foo (struct str * p_str)
{
  static int sum = 0;

  sum = sum + p_str->a;
  return sum;
}

int
main ()
{
  int i, sum;
  struct str * p = malloc (N * sizeof (struct str));

  for (i = 0; i < N; i++)
    sum = foo (p+i);

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "is passed to local function...Excluded." "ipa_struct_reorg" } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
