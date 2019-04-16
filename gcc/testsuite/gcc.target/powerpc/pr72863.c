/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O3" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "stxvd2x" } } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

#include <altivec.h>

extern unsigned char *src, *dst;

void b(void)
{
  int i;

  unsigned char *s8 = src;
  unsigned char *d8 = dst;

  for (i = 0; i < 100; i++) {
    vector unsigned char vs = vec_vsx_ld(0, s8);
    vector unsigned char vd = vec_vsx_ld(0, d8);
    vector unsigned char vr = vec_xor(vs, vd);
    vec_vsx_st(vr, 0, d8);
    s8 += 16;
    d8 += 16;
  }
}
