/* { dg-do compile } */
/* { dg-options "-mtune=thead-c906 -mno-strict-align" } */

int main() {

/* thead-c906 default is cpu tune param misaligned access fast */
#if !defined(__riscv_misaligned_fast)
#error "__riscv_misaligned_fast is not set"
#endif

#if defined(__riscv_misaligned_avoid) || defined(__riscv_misaligned_slow)
#error "__riscv_misaligned_avoid or __riscv_misaligned_slow is unexpectedly set"
#endif

  return 0;
}
