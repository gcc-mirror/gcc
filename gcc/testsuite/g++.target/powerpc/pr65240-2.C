/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 -ffast-math -mfp-in-toc -Wno-return-type" } */
/* { dg-additional-options "-mcmodel=small" { target lp64 } } */

/* target/65240, compiler got a 'insn does not satisfy its constraints' error.  */

#include "pr65240.h"
