/* { dg-do compile } */
/* { dg-options "-mriscv-attribute" } */
int foo()
{

/* In absence of -m[no-]strict-align, default mcpu is currently 
   set to rocket.  rocket has slow_unaligned_access=true.  */
#if !defined(__riscv_unaligned_slow)
#error "__riscv_unaligned_slow is not set"
#endif

#if defined(__riscv_unaligned_avoid) || defined(__riscv_unaligned_fast)
#error "__riscv_unaligned_avoid or __riscv_unaligned_fast is unexpectedly set"
#endif

return 0;
}
/* { dg-final { scan-assembler ".attribute arch" } } */
