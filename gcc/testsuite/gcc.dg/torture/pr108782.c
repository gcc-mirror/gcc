/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-copy-prop" } */

int m;

__attribute__ ((simd)) int
foo (void)
{
  unsigned a;
  int b = 0;

  m = a = 1;
  while (a != 0)
    {
      b = m;
      m = 2;
      ++a;
    }

  return b;
}
