/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void
foo (unsigned char *x, short y)
{
  short i;

  i = 2;
  while (i < y)
    {
      x[i - 1] = x[i];
      i = i + 1;
    }
}
/* { dg-final { cleanup-tree-dump "vect" } } */
