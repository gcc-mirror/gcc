/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64im_zve32x -mabi=lp64 -mcmodel=medlow -misa-spec=2.2" } */

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

#if defined(__riscv_e)
#error "__riscv_e"
#endif

#if !defined(__riscv_m)
#error "__riscv_m"
#endif

#if !defined(__riscv_zicsr)
#error "__riscv_zicsr"
#endif

#if !defined(__riscv_zve32x)
#error "__riscv_zve32x"
#endif

#if !defined(__riscv_zvl32b)
#error "__riscv_zvl32b"
#endif

#if !defined(__riscv_vector)
#error "__riscv_vector"
#endif

#if !defined(__riscv_v_min_vlen)
#error "__riscv_v_min_vlen"
#if __riscv_v_min_vlen != 32
#error "__riscv_v_elen != 32"
#endif
#endif

#if !defined(__riscv_v_elen)
#error "__riscv_v_elen"
#if __riscv_v_elen != 32
#error "__riscv_v_elen != 32"
#endif
#endif

#if !defined(__riscv_v_elen_fp)
#error "__riscv_v_elen_fp"
#if __riscv_v_elen_fp != 0
#error "__riscv_v_elen_fp != 0"
#endif
#endif

  return 0;
}
