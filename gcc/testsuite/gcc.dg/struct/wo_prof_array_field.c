/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>
typedef struct basic
{
  int a;
  int b[10];
} type_struct;

type_struct *str1;

int main()
{
  int i;

  str1 = malloc (10 * sizeof (type_struct));

  for (i=0; i<=9; i++)
    str1[i].a = str1[i].b[0];

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "Number of structures to transform is 1" "ipa_struct_reorg" { xfail *-*-* } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
