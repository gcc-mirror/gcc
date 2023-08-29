/* { dg-do compile } */
/* { dg-options "-mtune=rocket" } */

int main() {

/* rocket default is cpu tune param unaligned access slow */
#if !defined(__riscv_unaligned_slow)
#error "__riscv_unaligned_slow is not set"
#endif

#if defined(__riscv_unaligned_avoid) || defined(__riscv_unaligned_fast)
#error "__riscv_unaligned_avoid or __riscv_unaligned_fast is unexpectedly set"
#endif

  return 0;
}
