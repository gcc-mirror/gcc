/* { dg-do compile } */
/* { dg-options "-march=rv64g_xandesvdot -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xandesvdot)
#error "__riscv_xandesvdot"
#endif

  return 0;
}
