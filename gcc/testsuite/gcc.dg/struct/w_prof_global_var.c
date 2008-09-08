#include <stdlib.h>
typedef struct
{
  int a;
  float b;
}str_t;

#ifdef STACK_SIZE
#if STACK_SIZE > 8000
#define N 1000
#else
#define N (STACK_SIZE/8)
#endif
#else
#define N 1000
#endif

str_t *p;

int
main ()
{
  int i, sum;

  p = malloc (N * sizeof (str_t));
  if (p == NULL)
    return 0;
  for (i = 0; i < N; i++)
    p[i].b = i;

  for (i = 0; i < N; i++)
    p[i].a = p[i].b + 1;

  for (i = 0; i < N; i++)
    if (p[i].a != p[i].b + 1)
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final-use { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail { "avr-*-*" } } } } */
/* { dg-final-use { cleanup-ipa-dump "*" } } */
