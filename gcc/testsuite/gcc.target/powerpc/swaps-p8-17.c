/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O1" } */
/* { dg-final { scan-assembler "lxvd2x" } } */
/* { dg-final { scan-assembler "xxpermdi" } } */

/* Verify that we don't try to do permute removal in the presence of
   vec_ste.  This used to ICE.  */
#include <altivec.h>

void f (void *p)
{
  vector unsigned int u32 = vec_vsx_ld (1, (const unsigned int *)p);
  vec_ste (u32, 1, (unsigned int *)p);
}
