/* { dg-do compile } */
/* { dg-options "-march=rv32em -mabi=ilp32e -mno-div -mcmodel=medlow -fno-pie" } */

int main () {
#if !defined(__riscv)
#error "__riscv"
#endif

#if defined(__riscv_compressed)
#error "__riscv_compressed"
#endif

#if !defined(__riscv_32e)
#error "__riscv_32e"
#endif
#if defined(__riscv_64e)
#error "__riscv_64e"
#endif

#if defined(__riscv_atomic)
#error "__riscv_atomic"
#endif

#if !defined(__riscv_mul)
#error "__riscv_mul"
#endif
#if defined(__riscv_div)
#error "__riscv_div"
#endif
#if defined(__riscv_muldiv)
#error "__riscv_muldiv"
#endif

#if __riscv_xlen != 32
#error "__riscv_xlen"
#endif

#if defined(__riscv_fdiv)
#error "__riscv_fdiv"
#endif
#if defined(__riscv_fsqrt)
#error "__riscv_fsqrt"
#endif

#if !defined(__riscv_abi_rve)
#error "__riscv_abi_rve"
#endif
#if !defined(__riscv_float_abi_soft)
#error "__riscv_float_abi_soft"
#endif
#if defined(__riscv_float_abi_single)
#error "__riscv_float_abi_single"
#endif
#if defined(__riscv_float_abi_double)
#error "__riscv_float_abi_double"
#endif

#if !defined(__riscv_cmodel_medlow)
#error "__riscv_cmodel_medlow"
#endif
#if defined(__riscv_cmodel_medany)
#error "__riscv_cmodel_medlow"
#endif

  return 0;
}
