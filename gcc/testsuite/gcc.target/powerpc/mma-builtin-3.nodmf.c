/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=power10 -O2 -mno-dense-math" } */

#include "mma-builtin-3.h"

/* { dg-final { scan-assembler-times {\mxxmtacc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxmfacc\M} 1 } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 2 } } */
/* { dg-final { scan-assembler-not {\mlxvp\M} } } */
/* { dg-final { scan-assembler-not {\mstxvp\M} } } */
/* { dg-final { scan-assembler-times {\mxvcvspbf16\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcvbf16spn\M} 1 } } */
