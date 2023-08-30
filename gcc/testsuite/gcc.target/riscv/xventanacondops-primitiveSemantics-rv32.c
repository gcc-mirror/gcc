/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xventanacondops -mabi=ilp32d" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og"} } */

#include "zicond-primitiveSemantics.c"

/* { dg-final { scan-assembler-not "vt\\.maskc\t" } } */
/* { dg-final { scan-assembler-not "vt\\.maskcn\t" } } */
