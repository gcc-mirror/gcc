/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "no FPU" { *-*-* } { "-msoft-float" } { "" } } */
/* { dg-options "-mcpu=leon3 -mfix-ut699 -O2" } */

/* UT699: two back-to-back loads need a NOP in between.  An empty asm
   statement emits no instruction, so it does not separate the two loads.  */

int
load_int (const int *p, const int *q)
{
  int a = *p;
  __asm__ volatile ("" : : : "memory");
  int b = *q;
  return a + b;
}

/* UT699: a single-word load into an FP register, a load into its sibling
   register and a store of the first register within the next four
   instructions need a NOP after the first load.  */

void
load_fp_first (const float *p, float *r)
{
  float x = p[0];
  __asm__ volatile ("" : : : "memory");
  float y = p[1];
  r[0] = x;
  r[1] = y;
}

void
load_fp_window (const float *p, float *r)
{
  float x = p[0];
  float y = p[1];
  __asm__ volatile ("" : : : "memory");
  __asm__ volatile ("" : : : "memory");
  __asm__ volatile ("" : : : "memory");
  __asm__ volatile ("" : : : "memory");
  r[0] = x;
  r[1] = y;
}

/* { dg-final { scan-assembler-times "ld\t\[^\n\]*\n\tnop\n\tld\t" 3 } } */
