/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xt-c910v2" { target { rv64 } } } */
/* XuanTie C910v2 => rv64imafdc_zicbom_zicbop_zicboz_zicntr_zicond_zicsr_
zifencei _zihintntl_zihintpause_zihpm_zawrs_zfa_zfbfmin_zfh_zca_zcb_zcd_zba_
zbb_zbc_xtheadba_xtheadbb_xtheadbs_xtheadcmo_xtheadcondmov_xtheadfmemidx_
xtheadmac_xtheadmemidx_xtheadmempair_xtheadsync */

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
