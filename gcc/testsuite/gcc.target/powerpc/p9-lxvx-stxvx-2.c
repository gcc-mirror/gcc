/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O1" } */
/* { dg-final { scan-assembler "lxvx" } } */
/* { dg-final { scan-assembler "stvewx" } } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */

/* Verify we don't perform P8 load-vector fusion on P9.  */
#include <altivec.h>

void f (void *p)
{
  vector unsigned int u32 = vec_vsx_ld (1, (const unsigned int *)p);
  vec_ste (u32, 1, (unsigned int *)p);
}
