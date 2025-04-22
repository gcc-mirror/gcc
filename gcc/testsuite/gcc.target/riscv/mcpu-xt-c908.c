/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xt-c908" { target { rv64 } } } */
/* XuanTie C908 => rv64imafdc_zicbom_zicbop_zicboz_zicntr_zicsr_zifencei_
zihintpause_zihpm_zfh_zba_zbb_zbc_zbs_sstc_svinval_svnapot_svpbmt_xtheadba_
xtheadbb_xtheadbs_xtheadcmo_xtheadcondmov_xtheadfmemidx_xtheadmac_
xtheadmemidx_xtheadmempair_xtheadsync */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 64)		\
      && defined(__riscv_compressed)	\
      && defined(__riscv_zicbom)		\
      && defined(__riscv_zicbop)		\
      && defined(__riscv_zicboz)		\
      && defined(__riscv_zicntr)		\
      && defined(__riscv_zicsr)		\
      && defined(__riscv_zifencei)		\
      && defined(__riscv_zihintpause)		\
      && defined(__riscv_zihpm)		\
      && defined(__riscv_zfh)		\
      && defined(__riscv_zba)		\
      && defined(__riscv_zbb)		\
      && defined(__riscv_zbc)		\
      && defined(__riscv_zbs)		\
      && defined(__riscv_sstc)		\
      && defined(__riscv_svinval)		\
      && defined(__riscv_svnapot)		\
      && defined(__riscv_svpbmt)		\
      && defined(__riscv_xtheadba)		\
      && defined(__riscv_xtheadbb)		\
      && defined(__riscv_xtheadbs)		\
      && defined(__riscv_xtheadcmo)		\
      && defined(__riscv_xtheadcondmov)		\
      && defined(__riscv_xtheadfmemidx)		\
      && defined(__riscv_xtheadmac)		\
      && defined(__riscv_xtheadmemidx)		\
      && defined(__riscv_xtheadmempair)		\
      && defined(__riscv_xtheadsync))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
