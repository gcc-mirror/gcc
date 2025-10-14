/* { dg-do compile } */
/* { dg-options "-march=rvb23s64 -mabi=lp64d" } */

int main () {

#ifndef __riscv_rvb23s64
#error "__riscv_rvb23s64"
#endif

  return 0;
}