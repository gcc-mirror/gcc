/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=7450 -fno-math-errno" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "" { has_arch_ppc64 } } */
/* { dg-final { scan-assembler-not {\mfctid\M} } }  */

/* powerpc 7450 doesn't support ppc64 (-m32 -mpowerpc64), so skips it.  */

#include "pr112707.h"
