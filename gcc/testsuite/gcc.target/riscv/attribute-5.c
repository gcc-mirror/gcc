/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -mno-strict-align" } */
int foo()
{

/* Default mcpu is rocket which has slow_unaligned_access=true.  */
#if !defined(__riscv_unaligned_slow)
#error "__riscv_unaligned_slow is not set"
#endif

#if defined(__riscv_unaligned_avoid) || defined(__riscv_unaligned_fast)
#error "__riscv_unaligned_avoid or __riscv_unaligned_fast is unexpectedly set"
#endif

return 0;
}
/* { dg-final { scan-assembler ".attribute unaligned_access, 1" } } */
