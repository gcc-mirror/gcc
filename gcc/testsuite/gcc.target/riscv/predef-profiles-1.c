/* { dg-do compile } */
/* { dg-options "-march=rvi20u64 -mabi=lp64" } */

int main () {

#ifndef __riscv_rvi20u64
#error "__riscv_rvi20u64"
#endif

  return 0;
}