/* { dg-do compile { target lp64 } } */
/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 -ffast-math -mcmodel=medium -Wno-return-type" } */

/* target/65240, compiler got a 'insn does not satisfy its constraints' error.  */

#include "pr65240.h"
