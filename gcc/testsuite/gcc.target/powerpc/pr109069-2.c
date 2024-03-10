/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* Disable rs6000 optimize_swaps as it drops some REG_EQUAL
   notes on const vector and affects test point here.  */
/* { dg-options "-O2 -mvsx -mno-optimize-swaps" } */

/* Verify we can optimize away vector shifting if every byte
   of vector is the same.  */

#include "pr109069-2.h"

/* { dg-final { scan-assembler-not {\mvsldoi\M} } } */
