/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops" } */

extern void bar () __attribute__ ((noreturn));

inline double
baz (double *x, unsigned int y)
{
  if (y >= 6)
    bar ();
  return x[y];
}

double *a, *b;

void
foo ()
{
  unsigned int r, s, t;

  for (r = 0; r < 2; r++)
    for (t = 0; t < 2; t++)
      {
        for (s = 0; s < 3; s++)
          b[r * 2 + t] += baz (a, 3 * s + t);
      }
}
