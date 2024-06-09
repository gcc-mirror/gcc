/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xiangshan-nanhu" { target { rv64 } } } */
/* XiangShan Nanhu => rv64imafdc_zba_zbb_zbc_zbs_zbkb_zbkc_zbkx_zknd
                      _zkne_zknh_zksed_zksh_svinval_zicbom_zicboz */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 64)		\
      && defined(__riscv_compressed)	\
      && defined(__riscv_zicbom)	\
      && defined(__riscv_zicboz)	\
      && defined(__riscv_zba)	\
      && defined(__riscv_zbb)	\
      && defined(__riscv_zbc)	\
      && defined(__riscv_zbs)	\
      && defined(__riscv_zbkb)	\
      && defined(__riscv_zbkc)	\
      && defined(__riscv_zbkx)	\
      && defined(__riscv_zknd)	\
      && defined(__riscv_zkne)	\
      && defined(__riscv_zknh)	\
      && defined(__riscv_zksed)	\
      && defined(__riscv_zksh)	\
      && defined(__riscv_svinval))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
