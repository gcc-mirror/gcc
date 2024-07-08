/* PR target/110040 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-final { scan-assembler-not {\mmfvsrd\M} } } */

#include <altivec.h>

void
foo (signed long *dst, vector signed __int128 src)
{
  *dst = (signed long) src[0];
}

