/* { dg-do compile } */
/* { dg-options "-O2 -fno-math-errno -mdejagnu-cpu=476fp" } */
/* { dg-require-effective-target ilp32 } */

/* powerpc 476fp has hard float enabled which is required by fctid */

#include "pr112707.h"

/* { dg-final { scan-assembler-times {\mfctid\M} 2 } } */
