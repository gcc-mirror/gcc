/* PR debug/46867 */
/* { dg-do compile } */
/* { dg-options "-O -g" } */

typedef __PTRDIFF_TYPE__ ptrdiff_t;

extern void *bar (void);

void
foo (int x, char *r1, char *r2, __INTPTR_TYPE__ *rp, char *t)
{
  char *tx = tx;
  char **cc = bar ();
  ptrdiff_t rx = r1 - r2;
  for (;;)
    {
      char *cp1 = (char *) *rp;
      char *cp2 = t;
      char *s;
      ptrdiff_t len = cp1 - tx + rx * (cp2 - cp1);
      while (len)
	;
      for (s = tx; s; s++)
	;
      while (s)
	if (s == cc[s - r1])
	  if (x)
	    bar ();
      if (cp1)
	tx = cp2;
    }
}
