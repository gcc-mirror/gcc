/* PR tree-optimization/128095 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-additional-options "-O3" } */

int printf (const char *, ...);
int a, b;

void
d (void)
{
  int e = -524288, f = 1, *g = &e;
  do
    {
      if (f == 0)
	{
	  if (!a)
	    e = 0;
	  goto k;
	}
    j:
      e = 5017 + *g;
      f = a;
    k:
      if (!((long) e << 44 >> 44))
	goto j;
    }
  while (238709 == 262143 - (-505837 ^ e));
  b = *g;
  printf ("%d", ((long) b << 44 >> 44));
}
