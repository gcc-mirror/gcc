/* { dg-do compile { target { powerpc64-*-* } } } */
/* { dg-skip-if "do not override mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O0" } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-final { scan-assembler-times "lxvx" 40 } } */
/* { dg-final { scan-assembler-times "stxvx" 40 } } */

#include <altivec.h>

extern vector double vd, *vdp;
extern vector signed long long vsll, *vsllp;
extern vector unsigned long long vull, *vullp;
extern vector float vf, *vfp;
extern vector signed int vsi, *vsip;
extern vector unsigned int vui, *vuip;
extern vector signed short vss, *vssp;
extern vector unsigned short vus, *vusp;
extern vector signed char vsc, *vscp;
extern vector unsigned char vuc, *vucp;
extern double *dp;
extern signed long long *sllp;
extern unsigned long long *ullp;
extern float *fp;
extern signed int *sip;
extern unsigned int *uip;
extern signed short *ssp;
extern unsigned short *usp;
extern signed char *scp;
extern unsigned char *ucp;

void foo0 (void)
{
  vd = vec_xl (0, vdp);
}

void foo1 (void)
{
  vsll = vec_xl (0, vsllp);
}

void foo2 (void)
{
  vull = vec_xl (0, vullp);
}

void foo3 (void)
{
  vf = vec_xl (0, vfp);
}

void foo4 (void)
{
  vsi = vec_xl (0, vsip);
}

void foo5 (void)
{
  vui = vec_xl (0, vuip);
}

void foo6 (void)
{
  vss = vec_xl (0, vssp);
}

void foo7 (void)
{
  vus = vec_xl (0, vusp);
}

void foo8 (void)
{
  vsc = vec_xl (0, vscp);
}

void foo9 (void)
{
  vuc = vec_xl (0, vucp);
}

void foo10 (void)
{
  vec_xst (vd, 0, vdp);
}

void foo11 (void)
{
  vec_xst (vsll, 0, vsllp);
}

void foo12 (void)
{
  vec_xst (vull, 0, vullp);
}

void foo13 (void)
{
  vec_xst (vf, 0, vfp);
}

void foo14 (void)
{
  vec_xst (vsi, 0, vsip);
}

void foo15 (void)
{
  vec_xst (vui, 0, vuip);
}

void foo16 (void)
{
  vec_xst (vss, 0, vssp);
}

void foo17 (void)
{
  vec_xst (vus, 0, vusp);
}

void foo18 (void)
{
  vec_xst (vsc, 0, vscp);
}

void foo19 (void)
{
  vec_xst (vuc, 0, vucp);
}

void foo20 (void)
{
  vd = vec_xl (0, dp);
}

void foo21 (void)
{
  vsll = vec_xl (0, sllp);
}

void foo22 (void)
{
  vull = vec_xl (0, ullp);
}

void foo23 (void)
{
  vf = vec_xl (0, fp);
}

void foo24 (void)
{
  vsi = vec_xl (0, sip);
}

void foo25 (void)
{
  vui = vec_xl (0, uip);
}

void foo26 (void)
{
  vss = vec_xl (0, ssp);
}

void foo27 (void)
{
  vus = vec_xl (0, usp);
}

void foo28 (void)
{
  vsc = vec_xl (0, scp);
}

void foo29 (void)
{
  vuc = vec_xl (0, ucp);
}

void foo30 (void)
{
  vec_xst (vd, 0, dp);
}

void foo31 (void)
{
  vec_xst (vsll, 0, sllp);
}

void foo32 (void)
{
  vec_xst (vull, 0, ullp);
}

void foo33 (void)
{
  vec_xst (vf, 0, fp);
}

void foo34 (void)
{
  vec_xst (vsi, 0, sip);
}

void foo35 (void)
{
  vec_xst (vui, 0, uip);
}

void foo36 (void)
{
  vec_xst (vss, 0, ssp);
}

void foo37 (void)
{
  vec_xst (vus, 0, usp);
}

void foo38 (void)
{
  vec_xst (vsc, 0, scp);
}

void foo39 (void)
{
  vec_xst (vuc, 0, ucp);
}
