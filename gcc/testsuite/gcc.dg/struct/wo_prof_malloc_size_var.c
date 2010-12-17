/* { dg-do compile } */
/* { dg-do run } */

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

int
main ()
{
  int i, num;

  num = rand();
  num = num > N ? N : num; 
  str_t * p = malloc (num * sizeof (str_t));

  if (p == 0)
    return 0;

  for (i = 0; i < num; i++)
    p[i].b = i;

  for (i = 0; i < num; i++)
    p[i].a = p[i].b + 1;

  for (i = 0; i < num; i++)
    if (p[i].a != p[i].b + 1)
      abort ();
  
  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
