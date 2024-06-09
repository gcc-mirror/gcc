/* { dg-do compile } */
/* { dg-options "-mriscv-attribute" } */
int foo()
{

/* In absence of -m[no-]strict-align, default mcpu is currently 
   set to rocket.  rocket has slow_misaligned_access=true.  */
#if !defined(__riscv_misaligned_slow)
#error "__riscv_misaligned_slow is not set"
#endif

#if defined(__riscv_misaligned_avoid) || defined(__riscv_misaligned_fast)
#error "__riscv_misaligned_avoid or __riscv_misaligned_fast is unexpectedly set"
#endif

return 0;
}
/* { dg-final { scan-assembler ".attribute arch" } } */
