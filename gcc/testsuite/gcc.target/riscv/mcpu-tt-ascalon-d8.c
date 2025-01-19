/* { dg-do compile } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=tt-ascalon-d8 -mabi=lp64d" } */
/* Tenstorrent tt-ascalon-d8 => rv64imafdcv_zic64b_zicbom_zicbop_zicboz_ziccamoa_ziccif_zicclsm_ziccrse_zicond_zicsr_zifencei_zihintntl_zihintpause_zimop_za64rs_zawrs_zfa_zfbfmin_zfh_zcb_zcmop_zba_zbb_zbs_zvbb_zvbc_zvfbfwma_zvfh_zvkng_zvl256b */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && (__riscv_flen == 64)		\
      && defined(__riscv_i)		\
      && defined(__riscv_m)		\
      && defined(__riscv_a)		\
      && defined(__riscv_f)		\
      && defined(__riscv_d)		\
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
      && defined(__riscv_zicond)	\
      && defined(__riscv_zicsr)		\
      && defined(__riscv_zifencei)	\
      && defined(__riscv_zihintntl)	\
      && defined(__riscv_zihintpause)	\
      && defined(__riscv_zimop)		\
      && defined(__riscv_za64rs)	\
      && defined(__riscv_zawrs)		\
      && defined(__riscv_zfa)		\
      && defined(__riscv_zfbfmin)	\
      && defined(__riscv_zfh)		\
      && defined(__riscv_zcb)		\
      && defined(__riscv_zcmop)		\
      && defined(__riscv_zba)		\
      && defined(__riscv_zbb)		\
      && defined(__riscv_zbs)		\
      && defined(__riscv_zvbb)		\
      && defined(__riscv_zvbc)		\
      && defined(__riscv_zvfbfwma)	\
      && defined(__riscv_zvfh)		\
      && defined(__riscv_zvkng)		\
      && defined(__riscv_zvl256b)	\
      && defined(__riscv_zmmul)		\
      && defined(__riscv_zalrsc)	\
      && defined(__riscv_zaamo)		\
      && defined(__riscv_zvkb)		\
      && defined(__riscv_zvkg)		\
      && defined(__riscv_zvkn)		\
      && defined(__riscv_zvknc)		\
      && defined(__riscv_zvkned)	\
      && defined(__riscv_zvknhb)	\
      && defined(__riscv_zvkt)		\
      && defined(__riscv_zca)		\
      && defined(__riscv_zcd)		\
      && defined(__riscv_zfhmin)	\
      && defined(__riscv_zvfbfmin)	\
      && defined(__riscv_zvl32b)	\
      && defined(__riscv_zvl64b)	\
      && defined(__riscv_zvl128b)	\
      && defined(__riscv_zve32f)	\
      && defined(__riscv_zve32x)	\
      && defined(__riscv_zve64f)	\
      && defined(__riscv_zve64d)	\
      && defined(__riscv_zve64x)	\
      )
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
