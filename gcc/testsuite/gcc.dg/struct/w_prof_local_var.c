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
  int i, sum;

  str_t * p = malloc (N * sizeof (str_t));

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
/* { dg-final-use { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" } } */
/* { dg-final-use { cleanup-ipa-dump "*" } } */
