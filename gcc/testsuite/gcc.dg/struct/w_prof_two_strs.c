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

#ifdef STACK_SIZE
#if STACK_SIZE > 16000
#define N 1000
#else
#define N (STACK_SIZE/16)
#endif
#else
#define N 1000
#endif

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
  num = r > N ? N : r; 
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
/* { dg-final-use { scan-ipa-dump "Number of structures to transform is 2" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final-use { cleanup-ipa-dump "*" } } */

