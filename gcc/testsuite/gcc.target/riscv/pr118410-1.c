/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gcb -mabi=lp64d" { target { rv64} } } */
/* { dg-options "-march=rv32gcb -mabi=ilp32" { target { rv32} } } */

long orlow(long x) { return x | ((1L << 24) - 1); }

/* { dg-final { scan-assembler-times "orn\t" 1 } } */
/* { dg-final { scan-assembler-not "addi\t" } } */
