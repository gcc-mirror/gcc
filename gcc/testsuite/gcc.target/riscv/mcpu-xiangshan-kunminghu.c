/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "-march given" { *-*-* } { "-march=*" } } */
/* { dg-options "-mcpu=xiangshan-kunminghu" } */
/* XiangShan Kunminghu => rv64imafdcbvh_sdtrig_sha_shcounterenw_shgatpa
                      _shlcofideleg_shtvala_shvsatpa_shvstvala_shvstvecd
                      _smaia_smcsrind_smdbltrp_smmpm_smnpm_smrnmi_smstateen
                      _ssaia_ssccptr_sscofpmf_sscounterenw_sscsrind_ssdbltrp
                      _ssnpm_sspm_ssstateen_ssstrict_sstc_sstvala_sstvecd
                      _ssu64xl_supm_svade_svbare_svinval_svnapot_svpbmt
                      _za64rs_zacas_zawrs_zba_zbb_zbc_zbkb_zbkc_zbkx_zbs_zcb
                      _zcmop_zfa_zfh_zfhmin_zic64b_zicbom_zicbop_zicboz_ziccif
                      _zicclsm_ziccrse_zicntr_zicond_zicsr_zifencei_zihintpause
                      _zihpm_zimop_zkn_zknd_zkne_zknh_zksed_zksh_zkt_zvbb
                      _zvfh_zvfhmin_zvkt_zvl128b_zvl32b_zvl64b */

#if !((__riscv_xlen == 64)		\
      && !defined(__riscv_32e)		\
      && defined(__riscv_mul)		\
      && defined(__riscv_atomic)	\
      && (__riscv_flen == 64)		\
      && defined(__riscv_compressed)	\
      && defined(__riscv_v)		\
      && defined(__riscv_zic64b)	\
      && defined(__riscv_zicbom)	\
      && defined(__riscv_zicbop)	\
      && defined(__riscv_zicboz)	\
      && defined(__riscv_ziccif)	\
      && defined(__riscv_zicclsm)	\
      && defined(__riscv_ziccrse)	\
      && defined(__riscv_zicntr)	\
      && defined(__riscv_zicond)	\
      && defined(__riscv_zicsr)	\
      && defined(__riscv_zifencei)	\
      && defined(__riscv_zihintpause)	\
      && defined(__riscv_zihpm)	\
      && defined(__riscv_zimop)	\
      && defined(__riscv_za64rs)	\
      && defined(__riscv_zacas)	\
      && defined(__riscv_zawrs)	\
      && defined(__riscv_zba)	\
      && defined(__riscv_zbb)	\
      && defined(__riscv_zbc)	\
      && defined(__riscv_zbs)	\
      && defined(__riscv_zbkb)	\
      && defined(__riscv_zbkc)	\
      && defined(__riscv_zbkx)	\
      && defined(__riscv_zcb)	\
      && defined(__riscv_zcmop)	\
      && defined(__riscv_zfa)	\
      && defined(__riscv_zfh)	\
      && defined(__riscv_zknd)	\
      && defined(__riscv_zkne)	\
      && defined(__riscv_zknh)	\
      && defined(__riscv_zksed)	\
      && defined(__riscv_zksh)	\
      && defined(__riscv_zkt)	\
      && defined(__riscv_zvbb)	\
      && defined(__riscv_zvfh)	\
      && defined(__riscv_zvkt)	\
      && defined(__riscv_sdtrig)	\
      && defined(__riscv_sha)		\
      && defined(__riscv_shlcofideleg)	\
      && defined(__riscv_smaia)	\
      && defined(__riscv_smcsrind)	\
      && defined(__riscv_smdbltrp)	\
      && defined(__riscv_smmpm)	\
      && defined(__riscv_smnpm)	\
      && defined(__riscv_smrnmi)	\
      && defined(__riscv_smstateen)	\
      && defined(__riscv_ssaia)	\
      && defined(__riscv_ssccptr)	\
      && defined(__riscv_sscofpmf)	\
      && defined(__riscv_sscounterenw)	\
      && defined(__riscv_sscsrind)	\
      && defined(__riscv_ssdbltrp)	\
      && defined(__riscv_ssnpm)	\
      && defined(__riscv_sspm)	\
      && defined(__riscv_ssstrict)	\
      && defined(__riscv_sstc)		\
      && defined(__riscv_sstvala)	\
      && defined(__riscv_sstvecd)	\
      && defined(__riscv_ssu64xl)	\
      && defined(__riscv_supm)		\
      && defined(__riscv_svade)	\
      && defined(__riscv_svbare)	\
      && defined(__riscv_svinval)	\
      && defined(__riscv_svnapot)	\
      && defined(__riscv_svpbmt))
#error "unexpected arch"
#endif

int main()
{
  return 0;
}
