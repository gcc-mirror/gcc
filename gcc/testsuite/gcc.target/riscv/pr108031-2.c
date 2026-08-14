/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d " { target { rv64 } } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d " { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

#include "pr108031.c"

/* { dg-final { scan-assembler-times "%hi" 2 } } */
/* { dg-final { scan-assembler-times "%lo" 2 } } */
