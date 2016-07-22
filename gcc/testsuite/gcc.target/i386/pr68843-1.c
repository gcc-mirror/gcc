/* { dg-do compile } */
/* { dg-options "-O2" } */

double
test ()
{
  double x = 1.0;
  asm ("fld %1" /* { dg-error "explicitly used regs must be grouped at top of stack" } */
       : "=&t" (x)
       : "u" (x));
  return x;
}
