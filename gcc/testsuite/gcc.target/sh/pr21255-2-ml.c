/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-mb" && "-m5*"} { "" } }  */
/* { dg-final { scan-assembler "mov @\\(4,r.\\),r.; mov @r.,r." } } */
double d;

double
f (void)
{
  double r;
  asm ("mov %S1,%S0; mov %R1,%R0" : "=&r" (r) : "m" (d));
  return r;
}
