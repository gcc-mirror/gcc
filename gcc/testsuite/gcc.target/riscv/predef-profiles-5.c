/* { dg-do compile } */
/* { dg-options "-march=rva23u64 -mabi=lp64d" } */

int main () {

#ifndef __riscv_rva23u64
#error "__riscv_rva23u64"
#endif

  return 0;
}