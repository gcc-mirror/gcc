/* Origin: Aldy Hernandez  <aldyh@redhat.com>  */

/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

long **longp;
int *var_int;
unsigned long **ulongp;
vector pixel *varpixel;
vector signed char *vecchar;
vector signed long *vecint;
vector signed short *vecshort;
vector unsigned char *vecuchar;
vector unsigned long *vecuint;
vector unsigned short *vecushort;
vector float *vecfloat;

int main ()
{
  *vecfloat++ = vec_andc(vecint[0], vecfloat[1]);
  *vecfloat++ = vec_andc(vecfloat[0], vecint[1]);
  *vecfloat++ = vec_vxor(vecint[0], vecfloat[1]);
  *vecfloat++ = vec_vxor(vecfloat[0], vecint[1]);
  *varpixel++ = vec_packpx(vecuint[0], vecuint[1]);
  *varpixel++ = vec_vpkpx(vecuint[0], vecuint[1]);
  *vecshort++ = vec_vmulosb(vecchar[0], vecchar[1]);
  *vecint++ = vec_ld(var_int[0], longp[1]);
  *vecint++ = vec_lde(var_int[0], longp[1]);
  *vecint++ = vec_ldl(var_int[0], longp[1]);
  *vecint++ = vec_lvewx(var_int[0], longp[1]);
  *vecint++ = vec_unpackh(vecshort[0]);
  *vecint++ = vec_unpackl(vecshort[0]);
  *vecushort++ = vec_andc(vecshort[0], vecushort[1]);
  *vecushort++ = vec_andc(vecushort[0], vecshort[1]);
  *vecushort++ = vec_vxor(vecshort[0], vecushort[1]);
  *vecushort++ = vec_vxor(vecushort[0], vecshort[1]);
  *vecuint++ = vec_ld(var_int[0], ulongp[1]);
  *vecuint++ = vec_lvx(var_int[0], ulongp[1]);
  *vecuint++ = vec_vmsumubm(vecuchar[0], vecuchar[1], vecuint[2]);
  *vecuchar++ = vec_xor(vecuchar[0], vecchar[1]);

  return 0;
}
