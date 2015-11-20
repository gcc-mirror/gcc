// PR tree-optimization/68157
// { dg-do compile }
// { dg-options "-Ofast" }

double a, b, c, d;
int h, foo ();

void
bar ()
{
  while (foo ())
    {
      double e = b * a * a;
      double f = b * d;
      if (h)
	c = e + f;
    }
}
