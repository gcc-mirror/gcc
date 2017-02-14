/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int foo1 (unsigned char a[], unsigned int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (unsigned char)(x >= 100 ? x - 100 : 0);
    }
  return x;
}
/* { dg-final { scan-tree-dump " = _.* \\+ 156" "optimized" } } */
