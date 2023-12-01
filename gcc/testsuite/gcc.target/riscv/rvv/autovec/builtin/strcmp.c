/* { dg-do compile { target { riscv_v } } } */
/* { dg-additional-options "-O3 -minline-strcmp" } */

int
__attribute__ ((noipa))
foo (const char *s, const char *t)
{
  return __builtin_strcmp (s, t);
}

/* { dg-final { scan-assembler-times "vle8ff" 2 } } */
/* { dg-final { scan-assembler-times "vfirst.m" 1 } } */
/* { dg-final { scan-assembler-times "vmor.m" 1 } } */
