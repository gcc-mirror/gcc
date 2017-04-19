/* { dg-do compile { target { sh*-*-* && nonpic } } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

double
f ()
{
  double r;

  asm ("mov %S1,%S0; mov %R1,%R0" : "=r" (r) : "i" (f));
/* { dg-error "invalid operand to %S" "" {target "sh*-*-*" }  .-1 } */
/* { dg-error "invalid operand to %R" "" {target "sh*-*-*" }  .-2 } */
  return r;
}
