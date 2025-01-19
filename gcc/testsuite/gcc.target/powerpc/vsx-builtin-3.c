/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler "xxsel" } } */
/* { dg-final { scan-assembler "vperm" } } */
/* { dg-final { scan-assembler "xvrdpi" } } */
/* { dg-final { scan-assembler "xvrdpic" } } */
/* { dg-final { scan-assembler "xvrdpim" } } */
/* { dg-final { scan-assembler "xvrdpip" } } */
/* { dg-final { scan-assembler "xvrdpiz" } } */
/* { dg-final { scan-assembler "xvrspi" } } */
/* { dg-final { scan-assembler "xvrspic" } } */
/* { dg-final { scan-assembler "xvrspim" } } */
/* { dg-final { scan-assembler "xvrspip" } } */
/* { dg-final { scan-assembler "xvrspiz" } } */
/* { dg-final { scan-assembler "xsrdpi" } } */
/* { dg-final { scan-assembler "xsrdpic" } } */
/* { dg-final { scan-assembler "xsrdpim\|frim" } } */
/* { dg-final { scan-assembler "xsrdpip\|frip" } } */
/* { dg-final { scan-assembler "xsrdpiz\|friz" } } */
/* { dg-final { scan-assembler "xsmaxdp" } } */
/* { dg-final { scan-assembler "xsmindp" } } */
/* { dg-final { scan-assembler "xxland" } } */
/* { dg-final { scan-assembler "xxlandc" } } */
/* { dg-final { scan-assembler "xxlnor" } } */
/* { dg-final { scan-assembler "xxlor" } } */
/* { dg-final { scan-assembler "xxlxor" } } */
/* { dg-final { scan-assembler "xvcmpeqdp" } } */
/* { dg-final { scan-assembler "xvcmpgtdp" } } */
/* { dg-final { scan-assembler "xvcmpgedp" } } */
/* { dg-final { scan-assembler "xvcmpeqsp" } } */
/* { dg-final { scan-assembler "xvcmpgtsp" } } */
/* { dg-final { scan-assembler "xvcmpgesp" } } */
/* { dg-final { scan-assembler "xxsldwi" } } */
/* { dg-final { scan-assembler-not "call" } } */
/* { dg-final { scan-assembler "xvcvsxdsp" } } */
/* { dg-final { scan-assembler "xvcvuxdsp" } } */

#include <altivec.h>

extern __vector long long bool bll[][4];
extern __vector unsigned long long ull[][4];
extern __vector int si[][4];
extern __vector short ss[][4];
extern __vector signed char sc[][4];
extern __vector float f[][4];
extern __vector unsigned int ui[][4];
extern __vector unsigned short us[][4];
extern __vector unsigned char uc[][4];
extern __vector __bool int bi[][4];
extern __vector __bool short bs[][4];
extern __vector __bool char bc[][4];
extern __vector __pixel p[][4];
#ifdef __VSX__
extern __vector double d[][4];
extern __vector long sl[][4];
extern __vector long long sll[][4];
extern __vector unsigned long ul[][4];
extern __vector __bool long bl[][4];
#endif

int do_sel(void)
{
  int i = 0;

  si[i][0] = vec_sel (si[i][1], si[i][2], ui[i][3]); i++;
  ss[i][0] = vec_sel (ss[i][1], ss[i][2], us[i][3]); i++;
  sc[i][0] = vec_sel (sc[i][1], sc[i][2], uc[i][3]); i++;
  f[i][0] = vec_sel (f[i][1], f[i][2], f[i][3]); i++;
  d[i][0] = vec_sel (d[i][1], d[i][2], d[i][3]); i++;

  si[i][0] = vec_sel (si[i][1], si[i][2], bi[i][3]); i++;
  ss[i][0] = vec_sel (ss[i][1], ss[i][2], bs[i][3]); i++;
  sc[i][0] = vec_sel (sc[i][1], sc[i][2], bc[i][3]); i++;
  f[i][0] = vec_sel (f[i][1], f[i][2], bi[i][3]); i++;
  d[i][0] = vec_sel (d[i][1], d[i][2], bl[i][3]); i++;

  si[i][0] = vec_sel (si[i][1], si[i][2], ui[i][3]); i++;
  ss[i][0] = vec_sel (ss[i][1], ss[i][2], us[i][3]); i++;
  sc[i][0] = vec_sel (sc[i][1], sc[i][2], uc[i][3]); i++;
  f[i][0] = vec_sel (f[i][1], f[i][2], ui[i][3]); i++;
  d[i][0] = vec_sel (d[i][1], d[i][2], ul[i][3]); i++;

  return i;
}

int do_perm(void)
{
  int i = 0;

  sll[i][0] = vec_perm (sll[i][1], sll[i][2], uc[i][3]); i++;
  bll[i][0] = vec_perm (bll[i][1], bll[i][2], uc[i][3]); i++;
  si[i][0] = vec_perm (si[i][1], si[i][2], uc[i][3]); i++;
  bi[i][0] = vec_perm (bi[i][1], bi[i][2], uc[i][3]); i++;
  ss[i][0] = vec_perm (ss[i][1], ss[i][2], uc[i][3]); i++;
  bs[i][0] = vec_perm (bs[i][1], bs[i][2], uc[i][3]); i++;
  sc[i][0] = vec_perm (sc[i][1], sc[i][2], uc[i][3]); i++;
  bc[i][0] = vec_perm (bc[i][1], bc[i][2], uc[i][3]); i++;
  p[i][0] = vec_perm (p[i][1], p[i][2], uc[i][3]); i++;
  f[i][0] = vec_perm (f[i][1], f[i][2], uc[i][3]); i++;
  d[i][0] = vec_perm (d[i][1], d[i][2], uc[i][3]); i++;

  ull[i][0] = vec_perm (ull[i][1], ull[i][2], uc[i][3]); i++;
  ui[i][0] = vec_perm (ui[i][1], ui[i][2], uc[i][3]); i++;
  us[i][0] = vec_perm (us[i][1], us[i][2], uc[i][3]); i++;
  uc[i][0] = vec_perm (uc[i][1], uc[i][2], uc[i][3]); i++;

  si[i][0] = vec_perm (si[i][1], si[i][2], uc[i][3]); i++;
  ss[i][0] = vec_perm (ss[i][1], ss[i][2], uc[i][3]); i++;
  sc[i][0] = vec_perm (sc[i][1], sc[i][2], uc[i][3]); i++;
  f[i][0] = vec_perm (f[i][1], f[i][2], uc[i][3]); i++;
  d[i][0] = vec_perm (d[i][1], d[i][2], uc[i][3]); i++;

  return i;
}

int do_xxperm (void)
{
  int i = 0;

  d[i][0] = __builtin_vsx_xxpermdi_2df (d[i][1], d[i][2], 0); i++;
  d[i][0] = __builtin_vsx_xxpermdi (d[i][1], d[i][2], 1); i++;
  return i;
}

double x, y;
void do_concat (void)
{
  d[0][0] = __builtin_vsx_concat_2df (x, y);
}

extern double z[][4];

int do_math (void)
{
  int i = 0;

  d[i][0] = __builtin_vsx_xvrdpi  (d[i][1]); i++;
  d[i][0] = __builtin_vsx_xvrdpic (d[i][1]); i++;
  d[i][0] = __builtin_vsx_xvrdpim (d[i][1]); i++;
  d[i][0] = __builtin_vsx_xvrdpip (d[i][1]); i++;
  d[i][0] = __builtin_vsx_xvrdpiz (d[i][1]); i++;

  f[i][0] = __builtin_vsx_xvrspi  (f[i][1]); i++;
  f[i][0] = __builtin_vsx_xvrspic (f[i][1]); i++;
  f[i][0] = __builtin_vsx_xvrspim (f[i][1]); i++;
  f[i][0] = __builtin_vsx_xvrspip (f[i][1]); i++;
  f[i][0] = __builtin_vsx_xvrspiz (f[i][1]); i++;

  z[i][0] = __builtin_vsx_xsrdpi  (z[i][1]); i++;
  z[i][0] = __builtin_vsx_xsrdpic (z[i][1]); i++;
  z[i][0] = __builtin_vsx_xsrdpim (z[i][1]); i++;
  z[i][0] = __builtin_vsx_xsrdpip (z[i][1]); i++;
  z[i][0] = __builtin_vsx_xsrdpiz (z[i][1]); i++;
  z[i][0] = __builtin_vsx_xsmaxdp (z[i][1], z[i][0]); i++;
  z[i][0] = __builtin_vsx_xsmindp (z[i][1], z[i][0]); i++;
  return i;
}

int do_cmp (void)
{
  int i = 0;

  /* The __builtin_vsx_xvcmp[gt|ge|eq]dp and __builtin_vsx_xvcmp[gt|ge|eq]sp
     have been removed in favor of the overloaded vec_cmpeq, vec_cmpgt and
     vec_cmpge built-ins.  The __builtin_vsx_xvcmp* builtins returned a vector
     result of the same type as the arguments.  The vec_cmp* built-ins return
     a vector of boolenas of the same size as the arguments.  Thus the result
     assignment must be to a boolean or cast to a boolean.  Test both cases.
  */
     
  d[i][0] = (vector double) vec_cmpeq (d[i][1], d[i][2]); i++;
  d[i][0] = (vector double) vec_cmpgt (d[i][1], d[i][2]); i++;
  d[i][0] = (vector double) vec_cmpge (d[i][1], d[i][2]); i++;
  bl[i][0] = vec_cmpeq (d[i][1], d[i][2]); i++;
  bl[i][0] = vec_cmpgt (d[i][1], d[i][2]); i++;
  bl[i][0] = vec_cmpge (d[i][1], d[i][2]); i++;

  f[i][0] = (vector float) vec_cmpeq (f[i][1], f[i][2]); i++;
  f[i][0] = (vector float) vec_cmpgt (f[i][1], f[i][2]); i++;
  f[i][0] = (vector float) vec_cmpge (f[i][1], f[i][2]); i++;
  bi[i][0] = vec_cmpeq (f[i][1], f[i][2]); i++;
  bi[i][0] = vec_cmpgt (f[i][1], f[i][2]); i++;
  bi[i][0] = vec_cmpge (f[i][1], f[i][2]); i++;
  return i;
}

int do_logical (void)
{
  int i = 0;

  si[i][0] = __builtin_vsx_xxland (si[i][1], si[i][2]); i++;
  si[i][0] = __builtin_vsx_xxlandc (si[i][1], si[i][2]); i++;
  si[i][0] = __builtin_vsx_xxlnor (si[i][1], si[i][2]); i++;
  si[i][0] = __builtin_vsx_xxlor (si[i][1], si[i][2]); i++;
  si[i][0] = __builtin_vsx_xxlxor (si[i][1], si[i][2]); i++;

  ss[i][0] = __builtin_vsx_xxland (ss[i][1], ss[i][2]); i++;
  ss[i][0] = __builtin_vsx_xxlandc (ss[i][1], ss[i][2]); i++;
  ss[i][0] = __builtin_vsx_xxlnor (ss[i][1], ss[i][2]); i++;
  ss[i][0] = __builtin_vsx_xxlor (ss[i][1], ss[i][2]); i++;
  ss[i][0] = __builtin_vsx_xxlxor (ss[i][1], ss[i][2]); i++;

  sc[i][0] = __builtin_vsx_xxland (sc[i][1], sc[i][2]); i++;
  sc[i][0] = __builtin_vsx_xxlandc (sc[i][1], sc[i][2]); i++;
  sc[i][0] = __builtin_vsx_xxlnor (sc[i][1], sc[i][2]); i++;
  sc[i][0] = __builtin_vsx_xxlor (sc[i][1], sc[i][2]); i++;
  sc[i][0] = __builtin_vsx_xxlxor (sc[i][1], sc[i][2]); i++;

  d[i][0] = __builtin_vsx_xxland (d[i][1], d[i][2]); i++;
  d[i][0] = __builtin_vsx_xxlandc (d[i][1], d[i][2]); i++;
  d[i][0] = __builtin_vsx_xxlnor (d[i][1], d[i][2]); i++;
  d[i][0] = __builtin_vsx_xxlor (d[i][1], d[i][2]); i++;
  d[i][0] = __builtin_vsx_xxlxor (d[i][1], d[i][2]); i++;

  f[i][0] = __builtin_vsx_xxland (f[i][1], f[i][2]); i++;
  f[i][0] = __builtin_vsx_xxlandc (f[i][1], f[i][2]); i++;
  f[i][0] = __builtin_vsx_xxlnor (f[i][1], f[i][2]); i++;
  f[i][0] = __builtin_vsx_xxlor (f[i][1], f[i][2]); i++;
  f[i][0] = __builtin_vsx_xxlxor (f[i][1], f[i][2]); i++;
  return i;
}

int do_xxsldwi (void)
{
  int i = 0;

  si[i][0] = __builtin_vsx_xxsldwi (si[i][1], si[i][2], 0); i++;
  ss[i][0] = __builtin_vsx_xxsldwi (ss[i][1], ss[i][2], 1); i++;
  sc[i][0] = __builtin_vsx_xxsldwi (sc[i][1], sc[i][2], 2); i++;
  ui[i][0] = __builtin_vsx_xxsldwi (ui[i][1], ui[i][2], 3); i++;
  us[i][0] = __builtin_vsx_xxsldwi (us[i][1], us[i][2], 0); i++;
  uc[i][0] = __builtin_vsx_xxsldwi (uc[i][1], uc[i][2], 1); i++;
  f[i][0] = __builtin_vsx_xxsldwi (f[i][1], f[i][2], 2); i++;
  d[i][0] = __builtin_vsx_xxsldwi (d[i][1], d[i][2], 3); i++;
  return i;
}

int do_xvcvsxdsp (void)
{
  int i = 0;

  f[i][0] = __builtin_vsx_xvcvsxdsp (sll[i][1]); i++;

  return i;
}

int do_xvcvuxdsp (void)
{
  int i = 0;

  f[i][0] = __builtin_vsx_xvcvuxdsp (ull[i][1]); i++;

  return i;
}

