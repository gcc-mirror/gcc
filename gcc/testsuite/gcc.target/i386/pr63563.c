/* PR tree-optimization/63563 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx2" } */

struct A { unsigned long a, b, c, d; } a[1024] = { { 0, 1, 2, 3 } }, b;

void
foo (void)
{
  int i;
  for (i = 0; i < 1024; i++)
    {
      a[i].a = a[i].b = a[i].c = b.c;
      if (a[i].d)
	a[i].d = b.d;
    }
}
