/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec -mabi=altivec" } */
/* { dg-final { scan-assembler "vcmpgtub" } } */
/* { dg-final { scan-assembler "vcmpgtsh" } } */
/* { dg-final { scan-assembler "vcmpgtsw" } } */

/* Verify a statement in the GCC Manual that vector type specifiers can
   omit "signed" or "unsigned", with the default being "signed" for int
   and short, and "unsigned" for char.  */

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
