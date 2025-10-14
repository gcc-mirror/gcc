/* { dg-do compile } */
/* { dg-options "-march=rva22u64 -mabi=lp64d" } */

int main () {

#ifndef __riscv_rva22u64
#error "__riscv_rva22u64"
#endif

  return 0;
}