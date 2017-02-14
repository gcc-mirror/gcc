/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int
foo (char *a, unsigned n)
{
    int i;
    a[0] = 0;
    for (i = 16; i < n; i++)
      a[i] = a[i-16];
}
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
