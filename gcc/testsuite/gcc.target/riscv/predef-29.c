/* { dg-do compile } */
/* { dg-options "-march=rv64i_smstateen -mabi=lp64 -mcmodel=medlow -misa-spec=20191213" } */

int main () {

#ifndef __riscv_arch_test
#error "__riscv_arch_test"
#endif

#if __riscv_xlen != 64
#error "__riscv_xlen"
#endif

#if !defined(__riscv_i) || (__riscv_i != (2 * 1000 * 1000 + 1 * 1000))
#error "__riscv_i"
#endif

#if defined(__riscv_e)
#error "__riscv_e"
#endif

#if !defined(__riscv_zicsr)
#error "__riscv_zicsr"
#endif

#if !defined(__riscv_smstateen)
#error "__riscv_smstateen"
#endif

#if !defined(__riscv_ssstateen)
#error "__riscv_ssstateen"
#endif

  return 0;
}
