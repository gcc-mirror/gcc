/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target has_arch_ppc64 } */

#include "pr106769.h"

/* { dg-final { scan-assembler {\mmfvsrwz\M} } } */
/* { dg-final { scan-assembler {\mstxsiwx\M} } } */
/* { dg-final { scan-assembler-not {\mrldicl\M} } } */
