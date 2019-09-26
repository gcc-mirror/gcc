/* { dg-do compile } */
/* { dg-options "-O3 -fomit-frame-pointer -fno-tree-vectorize" } */

void foo (unsigned int n, char *a, char *b)
{
  int i;

  for (i = 0; i <= n - 1; ++i)
    a[i] = b[i];
}
