/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2" } */
/* { dg-final { scan-assembler-times "mtvsrd" 4 } } */
/* { dg-final { scan-assembler-times "mfvsrd" 4 } } */

/* Check code generation for direct move for long types.  */

#define TYPE vector int

#include "direct-move.h"
