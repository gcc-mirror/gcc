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
  str_t sub_str;
  int c;
}str_with_substr_t;

int
main ()
{
  int i;
  str_with_substr_t A[N];

  for (i = 0; i < N; i++)
    A[i].sub_str.a = 5;

  for (i = 0; i < N; i++)
    if (A[i].sub_str.a != 5)
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "is a field in the structure" "ipa_struct_reorg" } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
