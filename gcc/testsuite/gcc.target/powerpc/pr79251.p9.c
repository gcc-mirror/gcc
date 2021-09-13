/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */

#include <stddef.h>
#include <altivec.h>
#include "pr79251.h"

/* { dg-final { scan-assembler-not {\mstxw\M} } } */
/* { dg-final { scan-assembler-times {\mlvsl\M} 10 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlvsr\M} 10 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxperm\M} 20 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxinsertw\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvinserth\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvinsertb\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 3 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mrlwinm\M} 10 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstb\M|\msth\M|\mstw\M|\mstfs\M|\mstfd\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 7 { target ilp32 } } } */

