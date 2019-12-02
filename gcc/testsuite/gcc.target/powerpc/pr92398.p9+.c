/* { dg-do compile { target { lp64 && p9+ } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mvsx" } */

/* { dg-final { scan-assembler-times {\mmtvsrdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 1 } } */
/* { dg-final { scan-assembler-not {\mld\M} } } */
/* { dg-final { scan-assembler-not {\mnot\M} } } */

/* Source code for the test in pr92398.h */
#include "pr92398.h"
