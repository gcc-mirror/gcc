/* { dg-do compile } */
/* { dg-options "-march=rv64g_xsfvfnrclipxfqf -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xsfvfnrclipxfqf)
#error "__riscv_xsfvfnrclipxfqf"
#endif

  return 0;
}
