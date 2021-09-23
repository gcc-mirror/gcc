/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */

#include <stddef.h>
#include <altivec.h>
#include "pr79251.h"

/* { dg-final { scan-assembler-not {\mstxw\M} } } */
/* { dg-final { scan-assembler-times {\mlvsl\M} 10 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlvsr\M} 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 20 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxpermdi\M} 10 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxsel\M} 7 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mrlwinm\M} 10 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxvw4x\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstb\M|\msth\M|\mstw\M|\mstfs\M|\mstfd\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxvw4x\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxvd2x\M} 7 { target ilp32 } } } */

