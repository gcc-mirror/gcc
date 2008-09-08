/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct
{
  int a;
  float b;
  int c;
  float d;
}str_t;

#ifdef STACK_SIZE
#if STACK_SIZE > 1600
#define N 100
#else
#define N (STACK_SIZE/16)
#endif
#else
#define N 100
#endif

int 
main ()
{
  int i;
  str_t *p = malloc (N * sizeof (str_t));
  if (p == NULL)
    return 0;
  for (i = 0; i < N; i++)
    p[i].a = 5;

  for (i = 0; i < N; i++)
    if (p[i].a != 5)      
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* The structure str_t is erroneously peeled into 4 structures instead of 2.  */
/* { dg-final { scan-ipa-dump "the number of new types is 2" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
