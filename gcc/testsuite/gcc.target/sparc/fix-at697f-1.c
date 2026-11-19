/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "no FPU" { *-*-* } { "-msoft-float" } { "" } } */
/* { dg-options "-mcpu=leon -mfix-at697f -O2" } */

/* AT697F: a single-word load into an odd-numbered FP register followed by
   a double-precision operation on the enclosing register needs a NOP in
   between.  An empty asm statement emits no instruction, so it does not
   separate the two instructions.  */

double
foo (double x, const float *p)
{
  union { double d; float f[2]; } u;

  u.d = x;
  u.f[1] = *p;
  __asm__ volatile ("" : : : "memory");
  return u.d + u.d;
}

/* { dg-final { scan-assembler "ld\t\[^\n\]*%f\[0-9\]*\[13579\]\n\tnop\n\tfaddd" } } */
