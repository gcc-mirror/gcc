/* PR tree-optimization/59519 */
/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int a, b, c, d;

void
foo (void)
{
  for (; d; d++)
    for (b = 0; b < 14; b++)
      {
	c |= 1;
	if (a)
	  break;
      }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
