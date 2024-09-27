/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32i_zawrs -mabi=ilp32 -mcmodel=medlow -misa-spec=20191213" } */

int main () {

#ifndef __riscv_arch_test
#error "__riscv_arch_test"
#endif

#if __riscv_xlen != 32
#error "__riscv_xlen"
#endif

#if !defined(__riscv_i)
#error "__riscv_i"
#endif

#if !defined(__riscv_zawrs)
#error "__riscv_zawrs"
#endif

#if !defined(__riscv_zalrsc)
#error "__riscv_zalrsc"
#endif

#if defined(__riscv_a)
#error "__riscv_a"
#endif

  return 0;
}
