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

/* The workaround for the lost FDIV or FSQRT result inserts two NOPs after
   the first division of the short sequence FDIVD, FADDD, FADDD, FDIVD.  An
   empty asm statement emits no instruction, so it does not count as an
   instruction of the sequence.  */

void
div_first (const double *p)
{
  double a = p[0], b = p[1], c = p[2], d = p[3];
  double e = p[4], f = p[5], g = p[6], h = p[7];
  __asm__ volatile ("" : : "e" (a), "e" (b), "e" (c), "e" (d),
			   "e" (e), "e" (f), "e" (g), "e" (h));
  double x = a / b;
  __asm__ volatile ("" : : "e" (x));
  double y = c + d;
  double z = e + f;
  double w = g / h;
  __asm__ volatile ("" : : "e" (x), "e" (y), "e" (z), "e" (w));
}

void
div_window (const double *p)
{
  double a = p[0], b = p[1], c = p[2], d = p[3];
  double e = p[4], f = p[5], g = p[6], h = p[7];
  __asm__ volatile ("" : : "e" (a), "e" (b), "e" (c), "e" (d),
			   "e" (e), "e" (f), "e" (g), "e" (h));
  double x = a / b;
  double y = c + d;
  __asm__ volatile ("" : : "e" (x), "e" (y));
  double z = e + f;
  double w = g / h;
  __asm__ volatile ("" : : "e" (x), "e" (y), "e" (z), "e" (w));
}

/* { dg-final { scan-assembler-times "fdivd\t\[^\n\]*\n\tnop\n\tnop\n\tfaddd" 2 } } */
/* { dg-final { scan-assembler "nop\n\tfadds" } } */
/* { dg-final { scan-assembler-not ":\n\tfadds" } } */
