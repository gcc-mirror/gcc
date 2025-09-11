/* { dg-do compile } */
/* { dg-options "-march=rv64g_xandesvsintload -mabi=lp64" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if !defined(__riscv_xandesvsintload)
#error "__riscv_xandesvsintload"
#endif

  return 0;
}
