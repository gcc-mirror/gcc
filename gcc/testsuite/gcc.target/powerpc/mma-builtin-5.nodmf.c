/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "mma-builtin-5.h"

/* { dg-final { scan-assembler-times {\mlxv\M} 8 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxmtacc\M} 3 } } */
