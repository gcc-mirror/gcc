/* PR rtl-optimization/84032 */
/* { dg-do compile } */
/* { dg-options "-O1 -fmodulo-sched" } */
/* { dg-additional-options "-mcpu=power6" { target { powerpc-*-* } } } */

void
yr (int cm)
{
  int ka = cm;

  for (;;)
    {
      short int m0;

      for (m0 = 0; m0 < 6; ++m0)
        {
          ka &= 1;
          cm *= 2;
        }

      ka = (ka == 0) ? cm : 0;
    }
}
