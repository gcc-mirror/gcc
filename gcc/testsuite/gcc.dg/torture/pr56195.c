/* PR rtl-optimization/56195 */
/* { dg-do assemble } */

int i, a, t, b, c, d, e, f, j, *h;

void
fn (void)
{
  if (b)
    {
      int *p, *q;
      char g;

      if (f++)
	for (;; e++);
    lbl:
      for (b = 0; b < 2; b++)
	t /= d + 1 ? : i || a < c < (d = f) ? : 1 | (j = 2);

      *p = g >= *q ^ c != a ^ *p;

      if (!e)
	{
	  q = p;
	  goto lbl;
	}
    }

  if (h++)
    goto lbl;
}
