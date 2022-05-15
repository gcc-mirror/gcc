/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O3 -ffast-math -Wno-return-type" } */

/* target/65240, compiler got a 'insn does not satisfy its constraints' error.  */

#include "pr65240.h"
