/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>

typedef struct test_struct
{
  int a;
  int b;
} type_struct;

typedef type_struct **struct_pointer2;

struct_pointer2 str1;

int main()
{
  int i, j;

  str1 = malloc (2 * sizeof (type_struct *));

  for (i = 0; i <= 1; i++)
    str1[i] = malloc (2 * sizeof (type_struct));

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
