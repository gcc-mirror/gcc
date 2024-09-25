/* PR target/110040 */
/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-not {\mmfvsrd\M} } } */

/* builtin vec_xst_trunc requires power10.  */

#include <altivec.h>

void
foo (signed int *dst, vector signed __int128 src)
{
  __builtin_vec_xst_trunc (src, 0, dst);
}
