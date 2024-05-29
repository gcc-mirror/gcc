/* PR target/93073 */
/* { dg-do compile } */
/* { dg-options "-mvsx -O1 -ffinite-math-only -fno-trapping-math" } */
/* { dg-require-effective-target powerpc_vsx } */

void bar (void);

void
foo (long double x, double y, double z)
{
  for (;;)
    {
      double a = x > 0.0 ? y : z;
      if (a == 0.0)
	bar ();
    }
}
