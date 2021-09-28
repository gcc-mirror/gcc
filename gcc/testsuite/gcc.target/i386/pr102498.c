/* PR target/102498 */
/* { dg-do run { target fenv } } */
/* { dg-options "-frounding-math" } */

#include <fenv.h>
#include <stdlib.h>

__attribute__((noipa)) long double
fldlg2 (void)
{
  return 0.3010299956639811952256464283594894482L;
}

__attribute__((noipa)) long double
fldln2 (void)
{
  return 0.6931471805599453094286904741849753009L;
}

__attribute__((noipa)) long double
fldl2e (void)
{
  return 1.4426950408889634073876517827983434472L;
}

__attribute__((noipa)) long double
fldl2t (void)
{
  return 3.3219280948873623478083405569094566090L;
}

__attribute__((noipa)) long double
fldpi (void)
{
  return 3.1415926535897932385128089594061862044L;
}

int
main ()
{
  long double a = fldlg2 ();
  long double b = fldln2 ();
  long double c = fldl2e ();
  long double d = fldl2t ();
  long double e = fldpi ();
  static int f[] = { FE_TONEAREST, FE_TOWARDZERO, FE_UPWARD, FE_DOWNWARD };
  int i;
  for (i = 0; i < 4; i++)
    {
      fesetround (f[i]);
      if (a != fldlg2 ()
	  || b != fldln2 ()
	  || c != fldl2e ()
	  || d != fldl2t ()
	  || e != fldpi ())
	abort ();
    }
  return 0;
}
