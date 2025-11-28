/* { dg-do compile } */
/* { dg-options "-march=rv64g_xsmtvdot -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xsmtvdot)
#error "__riscv_xsmtvdot"
#endif

  return 0;
}
