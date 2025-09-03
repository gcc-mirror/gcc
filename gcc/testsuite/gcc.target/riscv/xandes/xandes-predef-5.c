/* { dg-do compile } */
/* { dg-options "-march=rv64g_xandesvpackfph -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xandesvpackfph)
#error "__riscv_xandesvpackfph"
#endif

  return 0;
}
