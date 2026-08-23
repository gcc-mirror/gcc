/* { dg-options "-maltivec -mno-vsx" } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target powerpc_altivec } */

/* The 128-bit integer vector built-ins need VSX, as V1TImode is only
   available with VSX.  Verify there is no ICE but expected error
   messages instead.  */

#include <altivec.h>

extern vector signed __int128 res_vsq;
extern vector unsigned __int128 res_vuq;
extern vector bool __int128 res_vbq;

extern vector signed __int128 vsq;
extern vector unsigned __int128 vuq;
extern vector bool __int128 vbq;
extern vector unsigned char vuc;

void
test_vec_sel (void)
{
  res_vsq = vec_sel (vsq, vsq, vuq); /* { dg-error "'__builtin_altivec_vsel_1ti' requires the '-mvsx' option" } */
  res_vbq = vec_sel (vbq, vbq, vuq); /* { dg-error "'__builtin_altivec_vsel_1ti' requires the '-mvsx' option" } */
}

void
test_vec_perm (void)
{
  res_vsq = vec_perm (vsq, vsq, vuc); /* { dg-error "'__builtin_altivec_vperm_1ti' requires the '-mvsx' option" } */
  res_vuq = vec_perm (vuq, vuq, vuc); /* { dg-error "'__builtin_altivec_vperm_1ti_uns' requires the '-mvsx' option" } */
}

void
test_vec_sld (void)
{
  res_vsq = vec_sld (vsq, vsq, 3); /* { dg-error "'__builtin_altivec_vsldoi_v1ti' requires the '-mvsx' option" } */
  res_vuq = vec_sld (vuq, vuq, 3); /* { dg-error "'__builtin_altivec_vsldoi_v1ti' requires the '-mvsx' option" } */
}
