/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d" { target rv64 } } */
/* { dg-options "-O2 -march=rv32gc_zicond -mabi=ilp32" { target rv32 } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-O3" "-Og" } } */

#include "../i386/pr114277.c"

/* { dg-final { scan-assembler-not "czero" } } */

