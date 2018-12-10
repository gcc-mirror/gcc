/* { dg-do run } */
/* { dg-options "-fdump-tree-lower" } */

#include <string.h>
#include <stdlib.h>

int cmp1 (char *p)
{
  return strncmp (p, "fis", 4);
}
int cmp2 (char *q)
{
  return strncmp ("fis", q, 4);
}

int main ()
{

  char *p = "fish";
  char *q = "fis\0";

  if (cmp1 (p) == 0 || cmp2 (q) != 0)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "strcmp \\(" 2 "lower" } } */
