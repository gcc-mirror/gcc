/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64i_zvfh -mabi=lp64f -mcmodel=medlow -misa-spec=20191213" } */

int main () {

#ifndef __riscv_arch_test
#error "__riscv_arch_test"
#endif

#if __riscv_xlen != 64
#error "__riscv_xlen"
#endif

#if !defined(__riscv_i)
#error "__riscv_i"
#endif

#if !defined(__riscv_f)
#error "__riscv_f"
#endif

#if !defined(__riscv_zvfh)
#error "__riscv_zvfh"
#endif

#if !defined(__riscv_zfhmin)
#error "__riscv_zfhmin"
#endif

#if defined(__riscv_zvfhmin)
#error "__riscv_zvfhmin"
#endif

#if defined(__riscv_v)
#error "__riscv_v"
#endif

#if defined(__riscv_d)
#error "__riscv_d"
#endif

#if defined(__riscv_c)
#error "__riscv_c"
#endif

#if defined(__riscv_a)
#error "__riscv_a"
#endif

#if defined(__riscv_zfh)
#error "__riscv_zfh"
#endif

  return 0;
}
