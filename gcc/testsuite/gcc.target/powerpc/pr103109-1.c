/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-require-effective-target has_arch_ppc64 } */
/* { dg-final { scan-assembler-times {\mmaddld\M} 2 } } */
/* { dg-final { scan-assembler-times {\mmaddhd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmaddhdu\M} 1 } } */

#include "pr103109.h"
