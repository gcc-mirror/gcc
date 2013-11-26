/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2" } */
/* { dg-final { scan-assembler "mtvsrd" } } */
/* { dg-final { scan-assembler "mfvsrd" } } */
/* { dg-final { scan-assembler "xscvdpspn" } } */
/* { dg-final { scan-assembler "xscvspdpn" } } */

/* Check code generation for direct move for float types.  */

#define TYPE float
#define IS_FLOAT 1
#define NO_ALTIVEC 1
#define VSX_REG_ATTR "ww"

#include "direct-move.h"
