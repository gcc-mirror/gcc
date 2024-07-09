/* { dg-do compile } */
/* { dg-options "-march=rv64g_xsfcease -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xsfcease)
#error "__riscv_xsfvcp"
#endif

  return 0;
}
