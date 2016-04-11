/* { dg-options "-mgeneral-regs-only -march=armv8-a+simd+fp -O2" } */

int
test (void)
{
  return 1;
}

/* { dg-final { scan-assembler-times "\\.arch armv8-a\n" 1 } } */
