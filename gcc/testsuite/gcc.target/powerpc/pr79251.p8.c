/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -maltivec" } */

#include <stddef.h>
#include <altivec.h>
#include "pr79251.h"

TEST_VEC_INSERT_ALL (test)

/* { dg-final { scan-assembler-not {\mstxw\M} } } */
/* { dg-final { scan-assembler-times {\mlvsl\M} 10 } } */
/* { dg-final { scan-assembler-times {\mlvsr\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 20 } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 10 } } */
/* { dg-final { scan-assembler-times {\mxxsel\M} 7 } } */

