/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  float b;
}str_t;

#ifdef STACK_SIZE
#if STACK_SIZE > 16000
#define N 1000
#else
#define N (STACK_SIZE/16)
#endif
#else
#define N 1000
#endif

typedef struct 
{
  str_t * sub_str;
  int c;
}str_with_substr_t;

int
main (void)
{
  int i;
  str_with_substr_t A[N];
  str_t a[N];

  for (i=0; i < N; i++)
    A[i].sub_str = &(a[i]);

  for (i=0; i < N; i++)
    A[i].sub_str->a = 5;

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "is a field in the structure" "ipa_struct_reorg" } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
