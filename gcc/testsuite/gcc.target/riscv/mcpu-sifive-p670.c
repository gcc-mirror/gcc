/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=sifive-p670 -mabi=lp64d" } */
/* SiFive p670 => rv64imafdcv_za64rs_zic64b_zicbom_zicbop_zicboz_ziccamoa_ziccif_zicclsm_ziccrse_zicsr_zifencei_zihintntl_zihintpause_zihpm_zfhmin_zba_zbb_zbs_zvl128b_zvbb_zvknc_zvkng_zvksc_zvksg */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && (__riscv_flen == 64)		\
      && defined(__riscv_c)		\
      && defined(__riscv_za64rs)	\
      && defined(__riscv_zic64b)	\
      && defined(__riscv_zicbom)	\
      && defined(__riscv_zicbop)	\
      && defined(__riscv_zicboz)	\
      && defined(__riscv_ziccamoa)	\
      && defined(__riscv_ziccif)	\
      && defined(__riscv_zicclsm)	\
      && defined(__riscv_ziccrse)	\
      && defined(__riscv_zicsr)		\
      && defined(__riscv_zifencei)	\
      && defined(__riscv_zihintntl)	\
      && defined(__riscv_zihintpause)	\
      && defined(__riscv_zihpm)		\
      && defined(__riscv_zfhmin)	\
      && defined(__riscv_zba)		\
      && defined(__riscv_zbb)		\
      && defined(__riscv_zbs)		\
      && defined(__riscv_zvl128b)	\
      && defined(__riscv_zvbb)		\
      && defined(__riscv_zvknc)		\
      && defined(__riscv_zvkng)		\
      && defined(__riscv_zvksc)		\
      && defined(__riscv_zvksg))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
