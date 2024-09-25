/* { dg-do compile } */
/* { dg-options "-march=rv64im_zve64f -mabi=lp64 -mcmodel=medlow -misa-spec=2.2" } */

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

#if defined(__riscv_c)
#error "__riscv_c"
#endif

#if defined(__riscv_e)
#error "__riscv_e"
#endif

#if defined(__riscv_a)
#error "__riscv_a"
#endif

#if !defined(__riscv_m)
#error "__riscv_m"
#endif

#if !defined(__riscv_f)
#error "__riscv_f"
#endif

#if defined(__riscv_d)
#error "__riscv_d"
#endif

#if defined(__riscv_v)
#error "__riscv_v"
#endif

#if !defined(__riscv_zvl32b)
#error "__riscv_zvl32b"
#endif

#if !defined(__riscv_zvl64b)
#error "__riscv_zvl64b"
#endif

#if defined(__riscv_zvl128b)
#error "__riscv_zvl128b"
#endif

#if defined(__riscv_zvl256b)
#error "__riscv_zvl256b"
#endif

#if defined(__riscv_zvl512b)
#error "__riscv_zvl512b"
#endif

#if defined(__riscv_zvl1024b)
#error "__riscv_zvl1024b"
#endif

#if !defined(__riscv_zve32x)
#error "__riscv_zve32x"
#endif

#if !defined(__riscv_zve32f)
#error "__riscv_zve32f"
#endif

#if !defined(__riscv_zve64x)
#error "__riscv_zve64x"
#endif

#if !defined(__riscv_zve64f)
#error "__riscv_zve64f"
#endif

#if defined(__riscv_zve64d)
#error "__riscv_zve64d"
#endif

  return 0;
}
