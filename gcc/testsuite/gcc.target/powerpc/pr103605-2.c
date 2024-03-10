/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx -ffast-math" } */
/* { dg-final { scan-assembler-times {\mxsmaxcdp\M} 3 { target has_arch_pwr9 } } } */
/* { dg-final { scan-assembler-times {\mxsmincdp\M} 3 { target has_arch_pwr9 } } } */
/* { dg-final { scan-assembler-times {\mxsmaxdp\M} 3 { target { ! has_arch_pwr9 } } } } */
/* { dg-final { scan-assembler-times {\mxsmindp\M} 3 { target { ! has_arch_pwr9 } } } } */

/* Verify that GCC generates expected min/max hw insns instead of fmin/fmax
   calls.  */
#include "pr103605.h"
