/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>

typedef struct
{
  int a;
  float b;
}str_t1;

typedef struct
{
  int c;
  float d;
}str_t2;

#define N 1000

str_t1 *p1;
str_t2 *p2;
int num;

void
foo (void)
{
  int i;

  for (i=0; i < num; i++)
    p2[i].c = 2;
}

int
main ()
{
  int i, r;

  r = rand ();
  num = r > N ? N : num; 
  p1 = malloc (num * sizeof (str_t1));
  p2 = malloc (num * sizeof (str_t2));

  if (p1 == NULL || p2 == NULL)
    return 0;

  for (i = 0; i < num; i++)
    p1[i].a = 1;

  foo ();

  for (i = 0; i < num; i++)
    if (p1[i].a != 1 || p2[i].c != 2)
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Number of structures to transform is 2" "ipa_struct_reorg" } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
