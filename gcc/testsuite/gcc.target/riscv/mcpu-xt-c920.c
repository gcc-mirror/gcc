/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xt-c920" { target { rv64 } } } */
/* XuanTie c920 => rv64imafdc_zicntr_zicsr_zifencei_zihpm_zfh_"xtheadba_xtheadbb_xtheadbs_xtheadcmo_xtheadcondmov_xtheadfmemidx_xtheadmac_xtheadmemidx_xtheadmempair_xtheadsync_xtheadvector */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 64)		\
      && defined(__riscv_compressed)	\
      && defined(__riscv_zicntr)		\
      && defined(__riscv_zicsr)		\
      && defined(__riscv_zifencei)		\
      && defined(__riscv_zihpm)		\
      && defined(__riscv_zfh)		\
      && defined(__riscv_xtheadba)		\
      && defined(__riscv_xtheadbb)		\
      && defined(__riscv_xtheadbs)		\
      && defined(__riscv_xtheadcmo)		\
      && defined(__riscv_xtheadcondmov)		\
      && defined(__riscv_xtheadfmemidx)		\
      && defined(__riscv_xtheadmac)		\
      && defined(__riscv_xtheadmemidx)		\
      && defined(__riscv_xtheadmempair)		\
      && defined(__riscv_xtheadsync)		\
      && defined(__riscv_xtheadvector))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
