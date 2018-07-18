/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_htm_ok } */
/* { dg-options "-O2 -mhtm" } */

/* { dg-final { scan-assembler "rlwinm r?\[0-9\]+,r?\[0-9\]+,3,30,31" { target { ilp32 } } } } */
/* { dg-final { scan-assembler "rldicl r?\[0-9\]+,r?\[0-9\]+,35,62" { target { lp64 } } } } */

#include <htmintrin.h>
long
ttest (void)
{
  return _HTM_STATE(__builtin_ttest());
}
