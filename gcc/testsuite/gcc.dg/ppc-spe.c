/* { dg-do compile { target powerpc-*-eabispe } } */
/* { dg-options "-mcpu=8540 -mabi=spe -O0" } */

/* (Test with -O0 so we don't optimize any of them away).  */

#include <spe.h>

/* Test PowerPC SPE extensions.  */

#define vector __attribute__((vector_size(8)))

vector int a, b, c, *ap;
vector float f, g, h;
unsigned int *uip;
unsigned short *usp;
int i, j, *ip;
uint64_t ull;
int64_t sll;
unsigned ui;
float fl;
uint16_t u16;
int16_t s16;

/* These are the only documented/supported accesor functions for the
   SPE builtins.  */
void
test_api ()
{
  c = __ev_addw(a, b);
  c = __ev_addiw(a, 8);
  c = __ev_subfw(a, b);
  c = __ev_subifw(a, 8);
  c = __ev_abs(a);
  c = __ev_neg(a);
  c = __ev_extsb(a);
  c = __ev_extsh(a);
  c = __ev_and(a, b);
  c = __ev_or(a, b);
  c = __ev_xor(a, b);
  c = __ev_nand(a, b);
  c = __ev_nor(a, b);
  c = __ev_eqv(a, b);
  c = __ev_andc(a, b);
  c = __ev_orc(a, b);
  c = __ev_rlw(a, b);
  c = __ev_rlwi(a, 8);
  c = __ev_slw(a, b);
  c = __ev_slwi(a, 8);
  c = __ev_srws(a, b);
  c = __ev_srwu(a, b);
  c = __ev_srwis(a, 8);
  c = __ev_srwiu(a, 8);
  c = __ev_cntlzw(a);
  c = __ev_cntlsw(a);
  c = __ev_rndw(a);
  c = __ev_mergehi(a, b);
  c = __ev_mergelo(a, b);
  c = __ev_mergelohi(a, b);
  c = __ev_mergehilo(a, b);
  c = __ev_splati(5);
  c = __ev_splatfi(6);
  c = __ev_divws(a, b);
  c = __ev_divwu(a, b);
  c = __ev_mra(a);
  i = __brinc(5, 6);

  /* Loads.  */
  c = __ev_lddx(ap, i);
  c = __ev_ldwx(ap, i);
  c = __ev_ldhx(ap, i);

  c = __ev_lwhex(uip, i);
  c = __ev_lwhoux(uip, i);
  c = __ev_lwhosx(uip, i);
  c = __ev_lwwsplatx(uip, i);
  c = __ev_lwhsplatx(uip, i);

  c = __ev_lhhesplatx(usp, i);
  c = __ev_lhhousplatx(usp, i);
  c = __ev_lhhossplatx(usp, i);

  c = __ev_ldd(ap, 5);
  c = __ev_ldw(ap, 6);
  c = __ev_ldh(ap, 7);
  c = __ev_lwhe(uip, 6);
  c = __ev_lwhou(uip, 6);
  c = __ev_lwhos(uip, 7);
  c = __ev_lwwsplat(uip, 7);
  c = __ev_lwhsplat(uip, 7);
  c = __ev_lhhesplat(usp, 7);
  c = __ev_lhhousplat(usp, 7);
  c = __ev_lhhossplat(usp, 7);

  /* Stores.  */
  __ev_stddx (a, ap, 9);
  __ev_stdwx (a, ap, 9);
  __ev_stdhx (a, ap, 9);
  __ev_stwwex (a, uip, 9);
  __ev_stwwox (a, uip, 9);
  __ev_stwhex (a, uip, 9);
  __ev_stwhox (a, uip, 9);
  __ev_stdd (a, ap, 9);
  __ev_stdw (a, ap, 9);
  __ev_stdh (a, ap, 9);
  __ev_stwwe (a, uip, 9);
  __ev_stwwo (a, uip, 9);
  __ev_stwhe (a, uip, 9);
  __ev_stwho (a, uip, 9);

  /* Fixed point complex.  */
  c = __ev_mhossf(a, b);
  c = __ev_mhosmf(a, b);
  c = __ev_mhosmi(a, b);
  c = __ev_mhoumi(a, b);
  c = __ev_mhessf(a, b);
  c = __ev_mhesmf(a, b);
  c = __ev_mhesmi(a, b);
  c = __ev_mheumi(a, b);
  c = __ev_mhossfa(a, b);
  c = __ev_mhosmfa(a, b);
  c = __ev_mhosmia(a, b);
  c = __ev_mhoumia(a, b);
  c = __ev_mhessfa(a, b);
  c = __ev_mhesmfa(a, b);
  c = __ev_mhesmia(a, b);
  c = __ev_mheumia(a, b);

  c = __ev_mhoumf (a, b);
  c = __ev_mheumf (a, b);
  c = __ev_mhoumfa (a, b);
  c = __ev_mheumfa (a, b);

  c = __ev_mhossfaaw(a, b);
  c = __ev_mhossiaaw(a, b);
  c = __ev_mhosmfaaw(a, b);
  c = __ev_mhosmiaaw(a, b);
  c = __ev_mhousiaaw(a, b);
  c = __ev_mhoumiaaw(a, b);
  c = __ev_mhessfaaw(a, b);
  c = __ev_mhessiaaw(a, b);
  c = __ev_mhesmfaaw(a, b);
  c = __ev_mhesmiaaw(a, b);
  c = __ev_mheusiaaw(a, b);
  c = __ev_mheumiaaw(a, b);

  c = __ev_mhousfaaw (a, b);
  c = __ev_mhoumfaaw (a, b);
  c = __ev_mheusfaaw (a, b);
  c = __ev_mheumfaaw (a, b);

  c = __ev_mhossfanw(a, b);
  c = __ev_mhossianw(a, b);
  c = __ev_mhosmfanw(a, b);
  c = __ev_mhosmianw(a, b);
  c = __ev_mhousianw(a, b);
  c = __ev_mhoumianw(a, b);
  c = __ev_mhessfanw(a, b);
  c = __ev_mhessianw(a, b);
  c = __ev_mhesmfanw(a, b);
  c = __ev_mhesmianw(a, b);
  c = __ev_mheusianw(a, b);
  c = __ev_mheumianw(a, b);

  c = __ev_mhousfanw (a, b);
  c = __ev_mhoumfanw (a, b);
  c = __ev_mheusfanw (a, b);
  c = __ev_mheumfanw (a, b);

  c = __ev_mhogsmfaa(a, b);
  c = __ev_mhogsmiaa(a, b);
  c = __ev_mhogumiaa(a, b);
  c = __ev_mhegsmfaa(a, b);
  c = __ev_mhegsmiaa(a, b);
  c = __ev_mhegumiaa(a, b);

  c = __ev_mhogumfaa (a, b);
  c = __ev_mhegumfaa (a, b);

  c = __ev_mhogsmfan(a, b);
  c = __ev_mhogsmian(a, b);
  c = __ev_mhogumian(a, b);
  c = __ev_mhegsmfan(a, b);
  c = __ev_mhegsmian(a, b);
  c = __ev_mhegumian(a, b);

  c = __ev_mhogumfan (a, b);
  c = __ev_mhegumfan (a, b);

  c = __ev_mwhssf(a, b);
  c = __ev_mwhsmf(a, b);
  c = __ev_mwhsmi(a, b);
  c = __ev_mwhumi(a, b);
  c = __ev_mwhssfa(a, b);
  c = __ev_mwhsmfa(a, b);
  c = __ev_mwhsmia(a, b);
  c = __ev_mwhumia(a, b);

  c = __ev_mwhumf (a, b);
  c = __ev_mwhumfa (a, b);

  c = __ev_mwlssf(a, b);
  c = __ev_mwlsmf(a, b);
  c = __ev_mwlumi(a, b);
  c = __ev_mwlssfa(a, b);
  c = __ev_mwlsmfa(a, b);
  c = __ev_mwlumia(a, b);
  c = __ev_mwlumiaaw(a, b);

  c = __ev_mwlufi (a, b);
  c = __ev_mwlufia (a, b);

  c = __ev_mwlssfaaw(a, b);
  c = __ev_mwlssiaaw(a, b);
  c = __ev_mwlsmfaaw(a, b);
  c = __ev_mwlsmiaaw(a, b);
  c = __ev_mwlusiaaw(a, b);
  c = __ev_mwlusiaaw(a, b);

  c = __ev_mwlusfaaw (a, b);

  c = __ev_mwlssfanw(a, b);
  c = __ev_mwlssianw(a, b);
  c = __ev_mwlsmfanw(a, b);
  c = __ev_mwlsmianw(a, b);
  c = __ev_mwlusianw(a, b);
  c = __ev_mwlumianw(a, b);

  c = __ev_mwlumfanw (a, b);
  c = __ev_mwlusfanw (a, b);

  c = __ev_mwssf (a, b);
  c = __ev_mwsmf (a, b);
  c = __ev_mwsmi (a, b);
  c = __ev_mwumi (a, b);
  c = __ev_mwssfa (a, b);
  c = __ev_mwsmfa (a, b);
  c = __ev_mwsmia (a, b);
  c = __ev_mwumia (a, b);
  c = __ev_mwumf (a, b);
  c = __ev_mwumfa (a, b);
  c = __ev_mwssfaa (a, b);
  c = __ev_mwsmfaa (a, b);
  c = __ev_mwsmiaa (a, b);
  c = __ev_mwumiaa (a, b);
  c = __ev_mwumfaa (a, b);
  c = __ev_mwssfan (a, b);
  c = __ev_mwsmfan (a, b);
  c = __ev_mwsmian (a, b);
  c = __ev_mwumian (a, b);
  c = __ev_mwumfan (a, b);
  c = __ev_addssiaaw (a);
  c = __ev_addsmiaaw (a);
  c = __ev_addusiaaw (a);
  c = __ev_addumiaaw (a);
  c = __ev_addusfaaw (a);
  c = __ev_addumfaaw (a);
  c = __ev_addsmfaaw (a);
  c = __ev_addssfaaw (a);
  c = __ev_subfssiaaw (a);
  c = __ev_subfsmiaaw (a);
  c = __ev_subfusiaaw (a);
  c = __ev_subfumiaaw (a);
  c = __ev_subfusfaaw (a);
  c = __ev_subfumfaaw (a);
  c = __ev_subfsmfaaw (a);
  c = __ev_subfssfaaw (a);

  /* Floating point SIMD instructions.  */
  c = __ev_fsabs (a);
  c = __ev_fsnabs (a);
  c = __ev_fsneg (a);
  c = __ev_fsadd (a, b);
  c = __ev_fssub (a, b);
  c = __ev_fsmul (a, b);
  c = __ev_fsdiv (a, b);
  c = __ev_fscfui (a);
  c = __ev_fscfsi (a);
  c = __ev_fscfuf (a);
  c = __ev_fscfsf (a);
  c = __ev_fsctui (a);
  c = __ev_fsctsi (a);
  c = __ev_fsctuf (a);
  c = __ev_fsctsf (a);
  c = __ev_fsctuiz (a);
  c = __ev_fsctsiz (a);

  /* Non supported sythetic instructions made from two instructions.  */

  c = __ev_mwhssfaaw (a, b);
  c = __ev_mwhssiaaw (a, b);
  c = __ev_mwhsmfaaw (a, b);
  c = __ev_mwhsmiaaw (a, b);
  c = __ev_mwhusiaaw (a, b);
  c = __ev_mwhumiaaw (a, b);
  c = __ev_mwhusfaaw (a, b);
  c = __ev_mwhumfaaw (a, b);
  c = __ev_mwhssfanw (a, b);
  c = __ev_mwhssianw (a, b);
  c = __ev_mwhsmfanw (a, b);
  c = __ev_mwhsmianw (a, b);
  c = __ev_mwhusianw (a, b);
  c = __ev_mwhumianw (a, b);
  c = __ev_mwhusfanw (a, b);
  c = __ev_mwhumfanw (a, b);

  c = __ev_mwhgssfaa (a, b);
  c = __ev_mwhgsmfaa (a, b);
  c = __ev_mwhgsmiaa (a, b);
  c = __ev_mwhgumiaa (a, b);
  c = __ev_mwhgssfan (a, b);
  c = __ev_mwhgsmfan (a, b);
  c = __ev_mwhgsmian (a, b);
  c = __ev_mwhgumian (a, b);

  /* Creating, insertion, and extraction.  */

  a = __ev_create_u64 ((uint64_t) 55);
  a = __ev_create_s64 ((int64_t) 66);
  a = __ev_create_fs (3.14F, 2.18F);
  a = __ev_create_u32 ((uint32_t) 5, (uint32_t) i);
  a = __ev_create_s32 ((int32_t) 5, (int32_t) 6);
  a = __ev_create_u16 ((uint16_t) 6, (uint16_t) 6, (uint16_t) 7, (uint16_t) 1);
  a = __ev_create_s16 ((int16_t) 6, (int16_t) 6, (int16_t) 7, (int16_t) 9);
  a = __ev_create_sfix32_fs (3.0F, 2.0F);
  a = __ev_create_ufix32_fs (3.0F, 2.0F);
  a = __ev_create_ufix32_u32 (3U, 5U);
  a = __ev_create_sfix32_s32 (6, 9);
  ull = __ev_convert_u64 (a);
  sll = __ev_convert_s64 (a);
  i = __ev_get_upper_u32 (a);
  ui = __ev_get_lower_u32 (a);
  i = __ev_get_upper_s32 (a);
  i = __ev_get_lower_s32 (a);
  fl = __ev_get_upper_fs (a);
  fl = __ev_get_lower_fs (a);
  u16 = __ev_get_u16 (a, 5U);
  s16 = __ev_get_s16 (a, 5U);
  ui = __ev_get_upper_ufix32_u32 (a);
  ui = __ev_get_lower_ufix32_u32 (a);
  i = __ev_get_upper_sfix32_s32 (a);
  i = __ev_get_lower_sfix32_s32 (a);
  fl = __ev_get_upper_sfix32_fs (a);
  fl = __ev_get_lower_sfix32_fs (a);
  fl = __ev_get_upper_ufix32_fs (a);
  fl = __ev_get_lower_ufix32_fs (a);
  a = __ev_set_upper_u32 (a, 5U);
  a = __ev_set_lower_u32 (a, 5U);
  a = __ev_set_upper_s32 (a, 5U);
  a = __ev_set_lower_s32 (a, 6U);
  a = __ev_set_upper_fs (a, 6U);
  a = __ev_set_lower_fs (a, fl);
  a = __ev_set_upper_ufix32_u32 (a, 5U);
  a = __ev_set_lower_ufix32_u32 (a, 5U);
  a = __ev_set_upper_sfix32_s32 (a, 5);
  a = __ev_set_lower_sfix32_s32 (a, 5);
  a =  __ev_set_upper_sfix32_fs (a, fl);
  a = __ev_set_lower_sfix32_fs (a, fl);
  a = __ev_set_upper_ufix32_fs (a, fl);
  a = __ev_set_lower_ufix32_fs (a, fl);
  a = __ev_set_acc_u64 ((uint64_t) 640);
  a = __ev_set_acc_s64 ((int64_t) 460);
  a = __ev_set_acc_vec64 (b);
  a = __ev_set_u32 (a, 5, 6);
  a = __ev_set_s32 (a, 5, 6);
  a = __ev_set_fs (a, fl, 5);
  a = __ev_set_u16 (a, 5U, 3);
  a = __ev_set_s16 (a, 5, 6);
  a = __ev_set_ufix32_u32 (a, 5U, 6U);
  a = __ev_set_sfix32_s32 (a, 3, 6);
  a = __ev_set_ufix32_fs (a, fl, 5);
  a = __ev_set_sfix32_fs (a, fl, 5);
  ui = __ev_get_u32 (a, 1);
  i = __ev_get_s32 (a, 0);
  fl = __ev_get_fs (a, 1);
  u16 = __ev_get_u16 (a, 2);
  s16 = __ev_get_s16 (a, 2);
  ui = __ev_get_ufix32_u32 (a, 1);
  i = __ev_get_sfix32_s32 (a, 0);
  fl = __ev_get_ufix32_fs (a, 1);
  fl = __ev_get_sfix32_fs (a, 0);

  /* Predicates.  */
  i = __ev_any_gts (a, b);
  i = __ev_all_gts (a, b);
  i = __ev_upper_gts (a, b);
  i = __ev_lower_gts (a, b);
  a = __ev_select_gts (a, b, c, c);

  i = __ev_any_gtu (a, b);
  i = __ev_all_gtu (a, b);
  i = __ev_upper_gtu (a, b);
  i = __ev_lower_gtu (a, b);
  a = __ev_select_gtu (a, b, c, c);

  i = __ev_any_lts (a, b);
  i = __ev_all_lts (a, b);
  i = __ev_upper_lts (a, b);
  i = __ev_lower_lts (a, b);
  a = __ev_select_lts (a, b, c, c);

  i = __ev_any_ltu (a, b);
  i = __ev_all_ltu (a, b);
  i = __ev_upper_ltu (a, b);
  i = __ev_lower_ltu (a, b);
  a = __ev_select_ltu (a, b, c, c);

  i = __ev_any_eq (a, b);
  i = __ev_all_eq (a, b);
  i = __ev_upper_eq (a, b);
  i = __ev_lower_eq (a, b);
  a = __ev_select_eq (a, b, c, c);

  i = __ev_any_fs_gt (a, b);
  i = __ev_all_fs_gt (a, b);
  i = __ev_upper_fs_gt (a, b);
  i = __ev_lower_fs_gt (a, b);
  a = __ev_select_fs_gt (a, b, c, c);

  i = __ev_any_fs_lt (a, b);
  i = __ev_all_fs_lt (a, b);
  i = __ev_upper_fs_lt (a, b);
  i = __ev_lower_fs_lt (a, b);
  a = __ev_select_fs_lt (a, b, c, b);

  i = __ev_any_fs_eq (a, b);
  i = __ev_all_fs_eq (a, b);
  i = __ev_upper_fs_eq (a, b);
  i = __ev_lower_fs_eq (a, b);
  a = __ev_select_fs_eq (a, b, c, c);

  i = __ev_any_fs_tst_gt (a, b);
  i = __ev_all_fs_tst_gt (a, b);
  i = __ev_upper_fs_tst_gt (a, b);
  i = __ev_lower_fs_tst_gt (a, b);
  a = __ev_select_fs_tst_gt (a, b, c, c);

  i = __ev_any_fs_tst_lt (a, b);
  i = __ev_all_fs_tst_lt (a, b);
  i = __ev_upper_fs_tst_lt (a, b);
  i = __ev_lower_fs_tst_lt (a, b);
  a = __ev_select_fs_tst_lt (a, b, c, c);

  i = __ev_any_fs_tst_eq (a, b);
  i = __ev_all_fs_tst_eq (a, b);
  i = __ev_upper_fs_tst_eq (a, b);
  i = __ev_lower_fs_tst_eq (a, b);
  a = __ev_select_fs_tst_eq (a, b, c, c);
}

int
main (void)
{
  /* Generic binary operations.  */
  c = __builtin_spe_evaddw (a, b);
  c = __builtin_spe_evand (a, b);
  c = __builtin_spe_evandc (a, b);
  c = __builtin_spe_evdivws (a, b);
  c = __builtin_spe_evdivwu (a, b);
  c = __builtin_spe_eveqv (a, b);
  h = __builtin_spe_evfsadd (f, g);
  h = __builtin_spe_evfsdiv (f, g);
  h = __builtin_spe_evfsmul (f, g);
  h = __builtin_spe_evfssub (f, g);
  c = __builtin_spe_evlddx (ap, j);
  c = __builtin_spe_evldhx (ap, j);
  c = __builtin_spe_evldwx (ap, j);
  c = __builtin_spe_evlhhesplatx (usp, j);
  c = __builtin_spe_evlhhossplatx (usp, j);
  c = __builtin_spe_evlhhousplatx (usp, j);
  c = __builtin_spe_evlwhex (uip, j);
  c = __builtin_spe_evlwhosx (uip, j);
  c = __builtin_spe_evlwhoux (uip, j);
  c = __builtin_spe_evlwhsplatx (uip, j);
  c = __builtin_spe_evlwwsplatx (uip, j);
  c = __builtin_spe_evmergehi (a, b);
  c = __builtin_spe_evmergehilo (a, b);
  c = __builtin_spe_evmergelo (a, b);
  c = __builtin_spe_evmergelohi (a, b);
  c = __builtin_spe_evmhegsmfaa (a, b);
  c = __builtin_spe_evmhegsmfan (a, b);
  c = __builtin_spe_evmhegsmiaa (a, b);
  c = __builtin_spe_evmhegsmian (a, b);
  c = __builtin_spe_evmhegumiaa (a, b);
  c = __builtin_spe_evmhegumian (a, b);
  c = __builtin_spe_evmhesmf (a, b);
  c = __builtin_spe_evmhesmfa (a, b);
  c = __builtin_spe_evmhesmfaaw (a, b);
  c = __builtin_spe_evmhesmfanw (a, b);
  c = __builtin_spe_evmhesmi (a, b);
  c = __builtin_spe_evmhesmia (a, b);
  c = __builtin_spe_evmhesmiaaw (a, b);
  c = __builtin_spe_evmhesmianw (a, b);
  c = __builtin_spe_evmhessf (a, b);
  c = __builtin_spe_evmhessfa (a, b);
  c = __builtin_spe_evmhessfaaw (a, b);
  c = __builtin_spe_evmhessfanw (a, b);
  c = __builtin_spe_evmhessiaaw (a, b);
  c = __builtin_spe_evmhessianw (a, b);
  c = __builtin_spe_evmheumi (a, b);
  c = __builtin_spe_evmheumia (a, b);
  c = __builtin_spe_evmheumiaaw (a, b);
  c = __builtin_spe_evmheumianw (a, b);
  c = __builtin_spe_evmheusiaaw (a, b);
  c = __builtin_spe_evmheusianw (a, b);
  c = __builtin_spe_evmhogsmfaa (a, b);
  c = __builtin_spe_evmhogsmfan (a, b);
  c = __builtin_spe_evmhogsmiaa (a, b);
  c = __builtin_spe_evmhogsmian (a, b);
  c = __builtin_spe_evmhogumiaa (a, b);
  c = __builtin_spe_evmhogumian (a, b);
  c = __builtin_spe_evmhosmf (a, b);
  c = __builtin_spe_evmhosmfa (a, b);
  c = __builtin_spe_evmhosmfaaw (a, b);
  c = __builtin_spe_evmhosmfanw (a, b);
  c = __builtin_spe_evmhosmi (a, b);
  c = __builtin_spe_evmhosmia (a, b);
  c = __builtin_spe_evmhosmiaaw (a, b);
  c = __builtin_spe_evmhosmianw (a, b);
  c = __builtin_spe_evmhossf (a, b);
  c = __builtin_spe_evmhossfa (a, b);
  c = __builtin_spe_evmhossfaaw (a, b);
  c = __builtin_spe_evmhossfanw (a, b);
  c = __builtin_spe_evmhossiaaw (a, b);
  c = __builtin_spe_evmhossianw (a, b);
  c = __builtin_spe_evmhoumi (a, b);
  c = __builtin_spe_evmhoumia (a, b);
  c = __builtin_spe_evmhoumiaaw (a, b);
  c = __builtin_spe_evmhoumianw (a, b);
  c = __builtin_spe_evmhousiaaw (a, b);
  c = __builtin_spe_evmhousianw (a, b);
  c = __builtin_spe_evmwhsmf (a, b);
  c = __builtin_spe_evmwhsmfa (a, b);
  c = __builtin_spe_evmwhsmi (a, b);
  c = __builtin_spe_evmwhsmia (a, b);
  c = __builtin_spe_evmwhssf (a, b);
  c = __builtin_spe_evmwhssfa (a, b);
  c = __builtin_spe_evmwhumi (a, b);
  c = __builtin_spe_evmwhumia (a, b);
  c = __builtin_spe_evmwlsmf (a, b);
  c = __builtin_spe_evmwlsmfa (a, b);
  c = __builtin_spe_evmwlsmfaaw (a, b);
  c = __builtin_spe_evmwlsmfanw (a, b);
  c = __builtin_spe_evmwlsmiaaw (a, b);
  c = __builtin_spe_evmwlsmianw (a, b);
  c = __builtin_spe_evmwlssf (a, b);
  c = __builtin_spe_evmwlssfa (a, b);
  c = __builtin_spe_evmwlssfaaw (a, b);
  c = __builtin_spe_evmwlssfanw (a, b);
  c = __builtin_spe_evmwlssiaaw (a, b);
  c = __builtin_spe_evmwlssianw (a, b);
  c = __builtin_spe_evmwlumi (a, b);
  c = __builtin_spe_evmwlumia (a, b);
  c = __builtin_spe_evmwlumiaaw (a, b);
  c = __builtin_spe_evmwlumianw (a, b);
  c = __builtin_spe_evmwlusiaaw (a, b);
  c = __builtin_spe_evmwlusianw (a, b);
  c = __builtin_spe_evmwsmf (a, b);
  c = __builtin_spe_evmwsmfa (a, b);
  c = __builtin_spe_evmwsmfaa (a, b);
  c = __builtin_spe_evmwsmfan (a, b);
  c = __builtin_spe_evmwsmi (a, b);
  c = __builtin_spe_evmwsmia (a, b);
  c = __builtin_spe_evmwsmiaa (a, b);
  c = __builtin_spe_evmwsmian (a, b);
  c = __builtin_spe_evmwssf (a, b);
  c = __builtin_spe_evmwssfa (a, b);
  c = __builtin_spe_evmwssfaa (a, b);
  c = __builtin_spe_evmwssfan (a, b);
  c = __builtin_spe_evmwumi (a, b);
  c = __builtin_spe_evmwumia (a, b);
  c = __builtin_spe_evmwumiaa (a, b);
  c = __builtin_spe_evmwumian (a, b);
  c = __builtin_spe_evnand (a, b);
  c = __builtin_spe_evnor (a, b);
  c = __builtin_spe_evor (a, b);
  c = __builtin_spe_evorc (a, b);
  c = __builtin_spe_evrlw (a, b);
  c = __builtin_spe_evslw (a, b);
  c = __builtin_spe_evsrws (a, b);
  c = __builtin_spe_evsrwu (a, b);
  c = __builtin_spe_evsubfw (a, b);
  c = __builtin_spe_evxor (a, b);
  /* GAS bug not implemented.
  c = __builtin_spe_evmwhssfaa (a, b);
  c = __builtin_spe_evmwhssmaa (a, b);
  c = __builtin_spe_evmwhsmfaa (a, b);
  c = __builtin_spe_evmwhsmiaa (a, b);
  c = __builtin_spe_evmwhusiaa (a, b);
  c = __builtin_spe_evmwhumiaa (a, b);
  c = __builtin_spe_evmwhssfan (a, b);
  c = __builtin_spe_evmwhssian (a, b);
  c = __builtin_spe_evmwhsmfan (a, b);
  c = __builtin_spe_evmwhsmian (a, b);
  c = __builtin_spe_evmwhusian (a, b);
  c = __builtin_spe_evmwhumian (a, b);
  c = __builtin_spe_evmwhgssfaa (a, b);
  c = __builtin_spe_evmwhgsmfaa (a, b);
  c = __builtin_spe_evmwhgsmiaa (a, b);
  c = __builtin_spe_evmwhgumiaa (a, b);
  c = __builtin_spe_evmwhgssfan (a, b);
  c = __builtin_spe_evmwhgsmfan (a, b);
  c = __builtin_spe_evmwhgsmian (a, b);
  c = __builtin_spe_evmwhgumian (a, b);
  */
  i = __builtin_spe_brinc (i, j);

  /* Generic unary operations.  */
  a = __builtin_spe_evabs (b);
  a = __builtin_spe_evaddsmiaaw (b);
  a = __builtin_spe_evaddssiaaw (b);
  a = __builtin_spe_evaddumiaaw (b);
  a = __builtin_spe_evaddusiaaw (b);
  a = __builtin_spe_evcntlsw (b);
  a = __builtin_spe_evcntlzw (b);
  a = __builtin_spe_evextsb (b);
  a = __builtin_spe_evextsh (b);
  f = __builtin_spe_evfsabs (g);
  f = __builtin_spe_evfscfsf (g);
  a = __builtin_spe_evfscfsi (g);
  f = __builtin_spe_evfscfuf (g);
  f = __builtin_spe_evfscfui (a);
  f = __builtin_spe_evfsctsf (g);
  a = __builtin_spe_evfsctsi (g);
  a = __builtin_spe_evfsctsiz (g);
  f = __builtin_spe_evfsctuf (g);
  a = __builtin_spe_evfsctui (g);
  a = __builtin_spe_evfsctuiz (g);
  f = __builtin_spe_evfsnabs (g);
  f = __builtin_spe_evfsneg (g);
  a = __builtin_spe_evmra (b);
  a = __builtin_spe_evneg (b);
  a = __builtin_spe_evrndw (b);
  a = __builtin_spe_evsubfsmiaaw (b);
  a = __builtin_spe_evsubfssiaaw (b);
  a = __builtin_spe_evsubfumiaaw (b);
  a = __builtin_spe_evsubfusiaaw (b);

  /* Unary operations of the form: X = foo (5_bit_signed_immediate).  */
  a = __builtin_spe_evsplatfi (5);
  a = __builtin_spe_evsplati (5);

  /* Binary operations of the form: X = foo(Y, 5_bit_immediate).  */
  a = __builtin_spe_evaddiw (b, 13);
  a = __builtin_spe_evldd (ap, 13);
  a = __builtin_spe_evldh (ap, 13);
  a = __builtin_spe_evldw (ap, 13);
  a = __builtin_spe_evlhhesplat (usp, 13);
  a = __builtin_spe_evlhhossplat (usp, 13);
  a = __builtin_spe_evlhhousplat (usp, 13);
  a = __builtin_spe_evlwhe (uip, 13);
  a = __builtin_spe_evlwhos (uip, 13);
  a = __builtin_spe_evlwhou (uip, 13);
  a = __builtin_spe_evlwhsplat (uip, 13);
  a = __builtin_spe_evlwwsplat (uip, 13);

  a = __builtin_spe_evrlwi (b, 13);
  a = __builtin_spe_evslwi (b, 13);
  a = __builtin_spe_evsrwis (b, 13);
  a = __builtin_spe_evsrwiu (b, 13);
  a = __builtin_spe_evsubifw (b, 13);

  /* Store indexed builtins.  */
  __builtin_spe_evstddx (b, ap, j);
  __builtin_spe_evstdhx (b, ap, j);
  __builtin_spe_evstdwx (b, ap, j);
  __builtin_spe_evstwhex (b, uip, j);
  __builtin_spe_evstwhox (b, uip, j);
  __builtin_spe_evstwwex (b, uip, j);
  __builtin_spe_evstwwox (b, uip, j);

  /* Store indexed immediate builtins.  */
  __builtin_spe_evstdd (b, ap, 5);
  __builtin_spe_evstdh (b, ap, 5);
  __builtin_spe_evstdw (b, ap, 5);
  __builtin_spe_evstwhe (b, uip, 5);
  __builtin_spe_evstwho (b, uip, 5);
  __builtin_spe_evstwwe (b, uip, 5);
  __builtin_spe_evstwwo (b, uip, 5);

  /* SPEFSCR builtins.  */
  i = __builtin_spe_mfspefscr ();
  __builtin_spe_mtspefscr (j);

  test_api ();
  
  return 0;
}
