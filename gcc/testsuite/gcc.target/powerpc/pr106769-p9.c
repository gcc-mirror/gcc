/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target has_arch_ppc64 } */

#include "pr106769.h"

/* { dg-final { scan-assembler {\mmfvsrwz\M} } } */
/* { dg-final { scan-assembler {\mstxsiwx\M} } } */
/* { dg-final { scan-assembler-not {\mrldicl\M} } } */
/* { dg-final { scan-assembler-not {\mxxextractuw\M} } } */
/* { dg-final { scan-assembler-not {\mvextuw[rl]x\M} } } */
