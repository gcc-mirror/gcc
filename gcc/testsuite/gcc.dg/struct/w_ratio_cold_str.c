#include <stdlib.h>
typedef struct
{
  int a;
  int b;
}str_t1;

typedef struct
{
  float a;
  float b;
}str_t2;

#define N1 1000
#define N2 100
str_t1 A1[N1];
str_t2 A2[N2];

int
main ()
{
  int i;

  for (i = 0; i < N1; i++)
    A1[i].a = 0;

  for (i = 0; i < N2; i++)
    A2[i].a = 0;

  for (i = 0; i < N1; i++)
    if (A1[i].a != 0) 
      abort ();

  for (i = 0; i < N2; i++)
    if (A2[i].a != 0) 
      abort ();

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final-use { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" } } */
/* { dg-final-use { cleanup-ipa-dump "*" } } */
