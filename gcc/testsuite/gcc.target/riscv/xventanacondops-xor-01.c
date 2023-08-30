/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

#include "zicond-xor-01.c"

/* { dg-final { scan-assembler-times "vt\\.maskc\t" 1 } } */
/* { dg-final { scan-assembler-times "xor\t" 1 } } */
