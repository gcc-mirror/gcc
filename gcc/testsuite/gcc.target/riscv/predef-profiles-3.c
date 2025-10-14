/* { dg-do compile } */
/* { dg-options "-march=rva20u64 -mabi=lp64d" } */

int main () {

#ifndef __riscv_rva20u64
#error "__riscv_rva20u64"
#endif

  return 0;
}