/* PR target/70465 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mfpmath=387 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "fxch\t%st.1" } } */

double
atan2 (double y, double x)
{
  double res = 0.0;
  asm ("fpatan" : "=t" (res) : "u" (y), "0" (x) : "st(1)");
  return res;
}
