/* { dg-do compile } */
/* { dg-options "-O2 -fno-math-errno -mdejagnu-cpu=power7" } */

/* -fno-math-errno is required to make {i,l,ll}rint{,f} inlined */

#include "pr88558.h"

/* { dg-final { scan-assembler-times {\mfctid\M} 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfctid\M} 2 { target { ilp32 && has_arch_ppc64 } } } } */
/* { dg-final { scan-assembler-times {\mfctiw\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfctiw\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstfiwx\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstfiwx\M} 4 { target ilp32 } } } */
