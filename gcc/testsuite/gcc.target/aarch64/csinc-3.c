/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize" } */

int f(int *a, int n, int *b, int d)
{
  for(int i = 0; i < n; i++)
    b[i] = a[i] == 100 ? 1 : d;
  /* { dg-final { scan-assembler "csinc\tw\[0-9\].*wzr" } } */
  return 0;
}
