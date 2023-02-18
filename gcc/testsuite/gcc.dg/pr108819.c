/* PR tree-optimization/108819 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-ccp -fno-tree-forwprop" } */

int a, b;

int
main ()
{
  int d = 1;
  for (; b; b++)
    if (a < 1)
      while (d <= a && a <= 0UL)
	{
	  int *e = &d;
	  *e = 0;
	}
  return 0;
}
