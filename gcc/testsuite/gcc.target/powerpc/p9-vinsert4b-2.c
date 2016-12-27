/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */

#include <altivec.h>

vector signed char
ins_v4si (vector int vi, vector signed char vc)
{
  return vec_vinsert4b (vi, vc, 13);	/* { dg-error "vec_vinsert4b" } */
}

vector unsigned char
ins_di (long di, vector unsigned char vc, long n)
{
  return vec_vinsert4b (di, vc, n);	/* { dg-error "vec_vinsert4b" } */
}

long
vext1 (vector signed char vc)
{
  return vec_vextract4b (vc, 13);	/* { dg-error "vec_vextract4b" } */
}

long
vextn (vector unsigned char vc, long n)
{
  return vec_vextract4b (vc, n);	/* { dg-error "vec_vextract4b" } */
}
