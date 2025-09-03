/* { dg-do compile } */
/* { dg-options "-march=rv64g_xandesperf -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xandesperf)
#error "__riscv_xandesperf"
#endif

  return 0;
}
