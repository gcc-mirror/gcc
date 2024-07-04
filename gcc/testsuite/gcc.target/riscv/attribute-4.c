/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -mstrict-align" } */
int foo()
{

#if !defined(__riscv_misaligned_avoid)
#error "__riscv_misaligned_avoid is not set"
#endif

#if defined(__riscv_misaligned_fast) || defined(__riscv_misaligned_slow)
#error "__riscv_misaligned_fast or __riscv_misaligned_slow is unexpectedly set"
#endif

  return 0;
}
/* { dg-final { scan-assembler ".attribute unaligned_access, 0" } } */
