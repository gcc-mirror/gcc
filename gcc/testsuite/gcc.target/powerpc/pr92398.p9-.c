/* { dg-do compile { target { lp64 && {! p9+} } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* { dg-final { scan-assembler-times {\mnot\M} 2 { xfail be } } } */
/* { dg-final { scan-assembler-times {\mstd\M} 2 { xfail { p8 && be } } } } */

/* Source code for the test in pr92398.h */
#include "pr92398.h"

