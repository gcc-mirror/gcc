/* PR target/93073 */
/* { dg-do compile { target powerpc_vsx_ok } } */
/* { dg-options "-mvsx -O1 -ffinite-math-only -fno-trapping-math" } */

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
