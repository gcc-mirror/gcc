/* { dg-do compile } */
/* { dg-options "-mtune=rocket -mstrict-align" } */

int main() {

#if !defined(__riscv_misaligned_avoid)
#error "__riscv_misaligned_avoid is not set"
#endif

#if defined(__riscv_misaligned_fast) || defined(__riscv_misaligned_slow)
#error "__riscv_misaligned_fast or __riscv_misaligned_slow is unexpectedly set"
#endif

  return 0;
}
