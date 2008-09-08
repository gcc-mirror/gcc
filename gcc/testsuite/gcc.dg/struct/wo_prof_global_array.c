/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  float b;
}str_t;

#define N 1000
str_t A[N];

int
main ()
{
  int i;

  for (i = 0; i < N; i++)
    {
      A[i].a = 0;
    }

  for (i = 0; i < N; i++)
    if (A[i].a != 0) 
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail { "avr-*-*" } } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
