/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough -O2" } */

/* Prevent false positive with optimizations.  */

extern void g (int);

void
f (int i)
{
  switch (i)
    {
    case 1:
      if (i > 10)
	g (0);
      else
	goto L;
      break;
L:
    case 2:;
    }
}
