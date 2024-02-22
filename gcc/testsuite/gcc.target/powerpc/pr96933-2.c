/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */

/* Test vector constructions with char/short type values whether use direct
   move instructions like mtvsrd/mtvsrwz on Power8, rather than transfering
   with memory store/load with stb/sth and vector load.  */

#include "pr96933.h"

/* { dg-final { scan-assembler-times {\mmtvsrd\M} 48 {target lp64 } } } */
/* { dg-final { scan-assembler-times {\mmtvsrwz\M} 48 {target {! lp64 } } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 4 } } */
/* { dg-final { scan-assembler-not {\mstb\M} } } */
/* { dg-final { scan-assembler-not {\msth\M} } } */
/* { dg-final { scan-assembler-not {\mrlwinm\M} } } */
