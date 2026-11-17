/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "no FPU" { *-*-* } { "-msoft-float" } { "" } } */
/* { dg-options "-mcpu=leon3 -mfix-gr712rc -O2" } */

/* GRLIB-TN-0012: a floating-point operation at the target of an integer
   branch needs a NOP before it, also where the epilogue starts at the
   branch target.  */

int a, b;

float
foo (float x)
{
  union { float f; int i; } u;

  u.f = x;
  a = u.i;
  if (u.i && b)
    return x + x;
  return x;
}

/* { dg-final { scan-assembler "nop\n\tfadds" } } */
/* { dg-final { scan-assembler-not ":\n\tfadds" } } */
