/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32i2p0 -mabi=ilp32 -mcmodel=medlow -misa-spec=2.2" } */

int main () {

#ifndef __riscv_arch_test
#error "__riscv_arch_test"
#endif

#if __riscv_xlen != 32
#error "__riscv_xlen"
#endif

#if !defined(__riscv_i) || (__riscv_i != (2 * 1000 * 1000))
#error "__riscv_i"
#endif

#if defined(__riscv_c)
#error "__riscv_c"
#endif

#if defined(__riscv_e)
#error "__riscv_e"
#endif

#if defined(__riscv_a)
#error "__riscv_a"
#endif

#if defined(__riscv_m)
#error "__riscv_m"
#endif

#if defined(__riscv_f)
#error "__riscv_f"
#endif

#if defined(__riscv_d)
#error "__riscv_d"
#endif

  return 0;
}
