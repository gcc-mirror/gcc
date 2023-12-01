/* { dg-do compile { target { riscv_v } } } */
/* { dg-additional-options "-O3 -minline-strncmp" } */

int
__attribute__ ((noipa))
foo (const char *s, const char *t)
{
  return __builtin_strncmp (s, t, 7);
}

/* { dg-final { scan-assembler-times "vle8ff" 2 } } */
/* { dg-final { scan-assembler-times "vfirst.m" 1 } } */
/* { dg-final { scan-assembler-times "vmor.m" 1 } } */
