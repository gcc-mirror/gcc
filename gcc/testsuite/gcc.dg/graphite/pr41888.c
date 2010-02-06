/* { dg-do compile } */
/* { dg-options "-g -O2 -ftree-loop-distribution -fgraphite-identity" } */

int
foo (int *x)
{
  int a[10], b[10];
  int i;
  a[9] = 8;
  b[9] = 8;
  for (i = 0; i < 9; i++)
    {
      a[i] = *x++;
      b[i] = 1;
    }
  b[i] = b[i] & !(a[i] ^ *x++);
  return b[i] ? i + 1 : 0;
}
