/* PR tree-optimization/59519 */
/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

struct S { int f0; } d;
int a[8] = { 0 }, b, c, e;

void
foo (void)
{
  for (; e < 1; e++)
    for (b = 0; b < 7; b++)
      {
	c |= (a[b + 1] != 0);
	if (d.f0)
	  break;
      }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
