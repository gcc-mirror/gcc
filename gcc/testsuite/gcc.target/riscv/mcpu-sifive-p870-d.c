/* { dg-do compile } */
/* { dg-options "-march=unset -mcpu=sifive-p870-d -mabi=lp64d" } */
/* SiFive p870-d => rv64imafdcbv_zic64b_zicbom_zicbop_zicboz_ziccamoa_ziccif_zicclsm_ziccrse_zicntr_zicond_zifencei_zihintntl_zihintpause_zihpm_zimop_za64rs_zama16b_zawrs_zfa_zfh_zcb_zcmop_zkr_zkt_zvbb_zvfbfwma_zvfh_zvfhmin_zvknc_zvksg */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && (__riscv_flen == 64)		\
      && defined(__riscv_c)		\
      && defined(__riscv_b)		\
      && defined(__riscv_v)		\
      && defined(__riscv_zic64b)	\
      && defined(__riscv_zicbom)	\
      && defined(__riscv_zicbop)	\
      && defined(__riscv_zicboz)	\
      && defined(__riscv_ziccamoa)	\
      && defined(__riscv_ziccif)	\
      && defined(__riscv_zicclsm)	\
      && defined(__riscv_ziccrse)	\
      && defined(__riscv_zicntr)	\
      && defined(__riscv_zicond)	\
      && defined(__riscv_zifencei)	\
      && defined(__riscv_zihintntl)	\
      && defined(__riscv_zihintpause)	\
      && defined(__riscv_zihpm)		\
      && defined(__riscv_zimop)		\
      && defined(__riscv_za64rs)	\
      && defined(__riscv_zama16b)	\
      && defined(__riscv_zawrs)		\
      && defined(__riscv_zfa)		\
      && defined(__riscv_zfh)		\
      && defined(__riscv_zcb)		\
      && defined(__riscv_zcmop)		\
      && defined(__riscv_zkr)		\
      && defined(__riscv_zkt)		\
      && defined(__riscv_zvbb)		\
      && defined(__riscv_zvfbfwma)	\
      && defined(__riscv_zvfh)		\
      && defined(__riscv_zvfhmin)	\
      && defined(__riscv_zvknc)		\
      && defined(__riscv_zvksg))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
