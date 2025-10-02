/* { dg-do compile } */
/* { dg-options "-march=rv64g_xandesbfhcvt -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xandesbfhcvt)
#error "__riscv_xandesbfhcvt"
#endif

  return 0;
}
