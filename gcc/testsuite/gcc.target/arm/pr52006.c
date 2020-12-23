/* PR target/52006 */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicts with multilib flags" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "no support for hard-float VFP ABI" { arm_thumb1 } { "-march=*" } { "" } } */
/* { dg-skip-if "-mpure-code and -fPIC incompatible" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-march=armv7-a+fp -mfloat-abi=hard -O2 -fPIC" } */
/* { dg-require-effective-target fpic } */

unsigned long a;
static int b;

void
foo (void)
{
  asm volatile ("" : "=r" (b));
}

void
bar (float f)
{
  if (f < b / 100.0)
    a = 1;
}
