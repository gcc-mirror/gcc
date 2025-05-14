/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xt-c920v2" { target { rv64 } } } */
/* XuanTie C920v2 => rv64imafdcv_zicbom_zicbop_zicboz_zicntr_zicond_zicsr_zifencei _zihintntl_zihintpause_zihpm_zawrs_zfa_zfbfmin_zfh_zca_zcb_zcd_zba_zbb_zbc_zbs_zvfbfmin_zvfbfwma_zvfh_sscofpmf_sstc_svinval_svnapot_svpbmt_xtheadba_xtheadbb_xtheadbs_xtheadcmo_xtheadcondmov_xtheadfmemidx_xtheadsync_xtheadvdot */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 64)		\
      && defined(__riscv_compressed)	\
      && defined(__riscv_v) \
      && defined(__riscv_zicbom)		\
      && defined(__riscv_zicbop)		\
      && defined(__riscv_zicboz)		\
      && defined(__riscv_zicntr)		\
      && defined(__riscv_zicond)		\
      && defined(__riscv_zicsr)		\
      && defined(__riscv_zifencei )		\
      && defined(__riscv_zihintntl)		\
      && defined(__riscv_zihintpause)		\
      && defined(__riscv_zihpm)		\
      && defined(__riscv_zawrs)		\
      && defined(__riscv_zfa)		\
      && defined(__riscv_zfbfmin)		\
      && defined(__riscv_zfh)		\
      && defined(__riscv_zca)		\
      && defined(__riscv_zcb)		\
      && defined(__riscv_zcd)		\
      && defined(__riscv_zba)		\
      && defined(__riscv_zbb)		\
      && defined(__riscv_zbc)		\
      && defined(__riscv_zbs)		\
      && defined(__riscv_zvfbfmin)		\
      && defined(__riscv_zvfbfwma)		\
      && defined(__riscv_zvfh)		\
      && defined(__riscv_sscofpmf)		\
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
      && defined(__riscv_xtheadsync)		\
      && defined(__riscv_xtheadvdot))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
