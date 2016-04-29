/* { dg-do compile { target { powerpc64le*-*-* } } } */
/* { dg-skip-if "do not override mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O0" } */
/* { dg-final { scan-assembler-times "lxvd2x" 18 } } */
/* { dg-final { scan-assembler-times "lxvw4x" 6 } } */
/* { dg-final { scan-assembler-times "stxvd2x" 18 } } */
/* { dg-final { scan-assembler-times "stxvw4x" 6 } } */
/* { dg-final { scan-assembler-times "xxpermdi" 24 } } */

#include <altivec.h>

extern vector double vd, *vdp;
extern vector signed long long vsll, *vsllp;
extern vector unsigned long long vull, *vullp;
extern vector float vf, *vfp;
extern vector signed int vsi, *vsip;
extern vector unsigned int vui, *vuip;
extern double *dp;
extern signed long long *sllp;
extern unsigned long long *ullp;
extern float *fp;
extern signed int *sip;
extern unsigned int *uip;

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
  vec_xst (vd, 0, vdp);
}

void foo7 (void)
{
  vec_xst (vsll, 0, vsllp);
}

void foo8 (void)
{
  vec_xst (vull, 0, vullp);
}

void foo9 (void)
{
  vec_xst (vf, 0, vfp);
}

void foo10 (void)
{
  vec_xst (vsi, 0, vsip);
}

void foo11 (void)
{
  vec_xst (vui, 0, vuip);
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
  vec_xst (vd, 0, dp);
}

void foo27 (void)
{
  vec_xst (vsll, 0, sllp);
}

void foo28 (void)
{
  vec_xst (vull, 0, ullp);
}

void foo29 (void)
{
  vec_xst (vf, 0, fp);
}

void foo30 (void)
{
  vec_xst (vsi, 0, sip);
}

void foo31 (void)
{
  vec_xst (vui, 0, uip);
}
