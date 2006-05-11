/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -mabi=altivec" } */
/* { dg-final { scan-assembler "vcmpgtub" { target *-*-linux* } } } */
/* { dg-final { scan-assembler "vcmpgtsb" { target *-*-darwin* } } } */
/* { dg-final { scan-assembler "vcmpgtsh" } } */
/* { dg-final { scan-assembler "vcmpgtsw" } } */

/* Verify a statement in the GCC Manual that vector type specifiers can
   omit "signed" or "unsigned".  The default is the default signedness
   of the base type, which differs depending on the ABI.  */

#include <altivec.h>

extern vector char vc1, vc2;
extern vector short vs1, vs2;
extern vector int vi1, vi2;

int signedness (void)
{
    return vec_all_le (vc1, vc2) 
           && vec_all_le (vs1, vs2)
           && vec_all_le (vi1, vi2);
}
