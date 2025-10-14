/* { dg-do compile } */
/* { dg-options "-march=rvb23u64 -mabi=lp64d" } */

int main () {

#ifndef __riscv_rvb23u64
#error "__riscv_rvb23u64"
#endif

  return 0;
}