/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 -ffast-math -mfp-in-toc -Wno-return-type" } */
/* { dg-additional-options "-mcmodel=small" { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx } */

/* target/65240, compiler got a 'insn does not satisfy its constraints' error.  */

#include "pr65240.h"
