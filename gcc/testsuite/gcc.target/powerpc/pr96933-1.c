/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

/* Test vector constructions with char/short type values whether use 128bit
   direct move instructions mtvsrdd on Power9 or later, rather than transfering
   with memory store/load with stb/sth and vector load.  */

#include "pr96933.h"

/* { dg-final { scan-assembler-times {\mmtvsrdd\M} 24 } } */
/* { dg-final { scan-assembler-times {\mvpkudum\M} 12 } } */
/* { dg-final { scan-assembler-not {\mstb\M} } } */
/* { dg-final { scan-assembler-not {\msth\M} } } */
/* { dg-final { scan-assembler-not {\mrlwinm\M} } } */
