/* { dg-do compile }  */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m2e" "-m3e" "*single-only" } { "" } }  */
/* { dg-final { scan-assembler "mov #?0,r.*; mov #?20,r" } } */
/* { dg-final { scan-assembler "mov #?1077149696,r.*; mov #?0,r" } } */
double
f ()
{
  double r;

  asm ("mov %S1,%S0; mov %R1,%R0" : "=r" (r) : "i" (20));
  asm ("mov %S1,%S0; mov %R1,%R0" : "+r" (r) : "i" (20.));
  return r;
}
