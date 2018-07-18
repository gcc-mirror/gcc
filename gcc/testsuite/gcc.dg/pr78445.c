/* PR tree-optimization/78445 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-if-convert -ftree-vectorize" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

int a;

void
foo (int x, int *y)
{
  while (a != 0)
    if (x != 0)
      {
	*y = a;
	x = *y;
      }
    else
      x = a;
}
