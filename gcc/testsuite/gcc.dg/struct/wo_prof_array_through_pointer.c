/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  float b;
}str_t;

#define N 1000

int
main ()
{
  int i;
  str_t A[N];
  str_t *p = A;

  for (i = 0; i < N; i++)
    p[i].a = 0;

  for (i = 0; i < N; i++)
    if (p[i].a != 0)
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* Currently str_t escapes due to incorrect ipa-type-escape analysis.  */
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
