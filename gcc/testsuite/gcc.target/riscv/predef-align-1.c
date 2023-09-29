/* { dg-do compile } */
/* { dg-options "-mtune=thead-c906" } */

int main() {

/* thead-c906 default is cpu tune param unaligned access fast */
#if !defined(__riscv_unaligned_fast)
#error "__riscv_unaligned_fast is not set"
#endif

#if defined(__riscv_unaligned_avoid) || defined(__riscv_unaligned_slow)
#error "__riscv_unaligned_avoid or __riscv_unaligned_slow is unexpectedly set"
#endif

  return 0;
}
