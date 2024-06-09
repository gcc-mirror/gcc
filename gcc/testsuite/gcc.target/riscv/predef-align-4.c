/* { dg-do compile } */
/* { dg-options "-mtune=rocket" } */

int main() {

/* rocket default is cpu tune param misaligned access slow */
#if !defined(__riscv_misaligned_slow)
#error "__riscv_misaligned_slow is not set"
#endif

#if defined(__riscv_misaligned_avoid) || defined(__riscv_misaligned_fast)
#error "__riscv_misaligned_avoid or __riscv_misaligned_fast is unexpectedly set"
#endif

  return 0;
}
