/* { dg-do compile { target { riscv_v } } } */
/* { dg-additional-options "-O3 -minline-strlen" } */

int
__attribute__ ((noipa))
foo (const char *s)
{
  return __builtin_strlen (s);
}

/* { dg-final { scan-assembler-times "vle8ff" 1 } } */
/* { dg-final { scan-assembler-times "vfirst.m" 1 } } */
