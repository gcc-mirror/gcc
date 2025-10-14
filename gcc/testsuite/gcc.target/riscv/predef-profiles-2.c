/* { dg-do compile } */
/* { dg-options "-march=rvi20u32 -mabi=ilp32" } */

int main () {

#ifndef __riscv_rvi20u32
#error "__riscv_rvi20u32"
#endif

  return 0;
}