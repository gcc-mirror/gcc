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
  int i, num;

  num = rand(); 
  str_t * p = malloc (num * sizeof (str_t));

  if (p == 0)
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
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
