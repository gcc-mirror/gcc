/* PR tree-optimization/92765 - wrong code for strcmp of a union member
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

__attribute__((noipa)) int
copy (char *x, int y)
{
  if (y == 0)
    strcpy (x, "abcd");
  return y;
}

__attribute__((noipa)) char *
alloc_2_copy_compare (int x)
{
  char *p;
  if (x)
    p = malloc (4);
  else
    p = calloc (16, 1);

  char *q = p + 2;
  if (copy (q, x))
    return p;

  if (strcmp (q, "abcd") != 0)
    abort ();

  return p;
}

char a5[5], a6[6], a7[7];

__attribute__((noipa)) char *
decl_3_copy_compare (int x)
{
  char *p = x < 0 ? a5 : 0 < x ? a6 : a7;
  char *q = p + 1;
  if (copy (q, x))
    return p;

  if (strcmp (q, "abcd") != 0)
    abort ();

  return p;
}

int main ()
{
  free (alloc_2_copy_compare (0));
  free (alloc_2_copy_compare (1));

  decl_3_copy_compare (-1);
  decl_3_copy_compare (0);
  decl_3_copy_compare (1);
}
