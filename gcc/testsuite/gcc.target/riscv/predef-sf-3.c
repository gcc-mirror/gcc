/* { dg-do compile } */
/* { dg-options "-march=rv64g_xsfvqmaccqoq -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xsfvqmaccqoq)
#error "__riscv_xsfvqmaccqoq"
#endif

  return 0;
}
