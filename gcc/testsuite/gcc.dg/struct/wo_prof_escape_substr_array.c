/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  float b;
}str_t;

#define N 1000

typedef struct 
{
  str_t A[N];
  int c;
}str_with_substr_t;

str_with_substr_t a;

int
main ()
{
  int i;
  
  for (i = 0; i < N; i++)
    a.A[i].b = 0;

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "is a field in the structure" "ipa_struct_reorg" } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
