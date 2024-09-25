/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64imv_zvfbfwma -mabi=lp64d -mcmodel=medlow -misa-spec=20191213" } */

int
main ()
{
#ifndef __riscv_arch_test
#error "__riscv_arch_test"
#endif

#if __riscv_xlen != 64
#error "__riscv_xlen"
#endif

#if !defined(__riscv_i)
#error "__riscv_i"
#endif

#if !defined(__riscv_m)
#error "__riscv_m"
#endif

#if !defined(__riscv_f)
#error "__riscv_f"
#endif

#if !defined(__riscv_d)
#error "__riscv_d"
#endif

#if !defined(__riscv_v)
#error "__riscv_v"
#endif

#if !defined(__riscv_zfbfmin)
#error "__riscv_zfbfmin"
#endif

#if !defined(__riscv_zvfbfwma)
#error "__riscv_zvfbfwma"
#endif

#if defined(__riscv_c)
#error "__riscv_c"
#endif

#if defined(__riscv_a)
#error "__riscv_a"
#endif

  return 0;
}
