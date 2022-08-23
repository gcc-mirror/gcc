/* { dg-do compile } */
/* { dg-options "-march=rv64i_zbkb_zbkc_zbkx_zknd_zkne_zknh_zksed_zksh_zkr_zkt -mabi=lp64 -mcmodel=medlow -misa-spec=2.2" } */

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

#if !defined(__riscv_zk)
#error "__riscv_zk"
#endif

#if !defined(__riscv_zkr)
#error "__riscv_zkr"
#endif

#if !defined(__riscv_zkn)
#error "__riscv_zkn"
#endif

#if !defined(__riscv_zks)
#error "__riscv_zks"
#endif

#if !defined(__riscv_zbkb)
#error "__riscv_zbkb"
#endif

#if !defined(__riscv_zbkc)
#error "__riscv_zbkc"
#endif

#if !defined(__riscv_zbkx)
#error "__riscv_zbkx"
#endif

#if !defined(__riscv_zknd)
#error "__riscv_zknd"
#endif

#if !defined(__riscv_zkne)
#error "__riscv_zkne"
#endif

#if !defined(__riscv_zknh)
#error "__riscv_zknh"
#endif

#if !defined(__riscv_zksh)
#error "__riscv_zksh"
#endif

  return 0;
}
