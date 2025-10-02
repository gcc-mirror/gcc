/* { dg-do compile } */
/* { dg-options "-march=rv64g_xandesvbfhcvt -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xandesvbfhcvt)
#error "__riscv_xandesvbfhcvt"
#endif

  return 0;
}
