/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-final { scan-assembler {\mmtvsrd\M}    } } */
/* { dg-final { scan-assembler {\mmfvsrwz\M}   } } */
/* { dg-final { scan-assembler {\mxscvdpspn\M} } } */
/* { dg-final { scan-assembler {\mxscvspdpn\M} } } */

/* Check code generation for direct move for float types.  */

#define TYPE float
#define IS_FLOAT 1
#define NO_ALTIVEC 1

#include "direct-move.h"
