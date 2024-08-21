/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-scev-cprop" } */

int a[3], d[3], c;
int f(int e, int b)
{
  for (; e < 3; e++)
    {
      a[0] = 0;
      if (b)
	c = b;
      d[e] = 0;
      a[e] = 0;
    }
  return e;
}
