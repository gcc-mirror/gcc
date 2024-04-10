/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-final { scan-assembler "mtvsrd" } } */
/* { dg-final { scan-assembler "mfvsrd" } } */

/* Check code generation for direct move for double types.  */

#define TYPE double
#define IS_FLOAT 1
#define NO_ALTIVEC 1

#include "direct-move.h"
