/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -mstrict-align" } */
int foo()
{

#if !defined(__riscv_unaligned_avoid)
#error "__riscv_unaligned_avoid is not set"
#endif

#if defined(__riscv_unaligned_fast) || defined(__riscv_unaligned_slow)
#error "__riscv_unaligned_fast or __riscv_unaligned_slow is unexpectedly set"
#endif

  return 0;
}
/* { dg-final { scan-assembler ".attribute unaligned_access, 0" } } */
