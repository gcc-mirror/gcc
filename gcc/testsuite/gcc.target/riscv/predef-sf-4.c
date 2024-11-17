/* { dg-do compile } */
/* { dg-options "-march=rv64g_xsfvqmaccdod -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xsfvqmaccdod)
#error "__riscv_xsfvqmaccdod"
#endif

  return 0;
}
