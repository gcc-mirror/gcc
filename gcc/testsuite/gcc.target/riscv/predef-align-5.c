/* { dg-do compile } */
/* { dg-options "-mtune=rocket -mstrict-align" } */

int main() {

#if !defined(__riscv_unaligned_avoid)
#error "__riscv_unaligned_avoid is not set"
#endif

#if defined(__riscv_unaligned_fast) || defined(__riscv_unaligned_slow)
#error "__riscv_unaligned_fast or __riscv_unaligned_slow is unexpectedly set"
#endif

  return 0;
}
