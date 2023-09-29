/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zknh -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto"} } */

#include "zknh-sha256-64.c"

/* { dg-final { scan-assembler-times "sha256sig0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sig1" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum0" 1 } } */
/* { dg-final { scan-assembler-times "sha256sum1" 1 } } */
