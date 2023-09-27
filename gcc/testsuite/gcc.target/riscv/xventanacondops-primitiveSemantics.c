/* { dg-do compile } */
/* { dg-options "-march=rv64gc_xventanacondops -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og"} } */

#include "zicond-primitiveSemantics.c"

/* { dg-final { scan-assembler-times "vt\\.maskc\t" 6 } } */
/* { dg-final { scan-assembler-times "vt\\.maskcn\t" 6 } } */
/* { dg-final { scan-assembler-not {\mbeq} } } */
/* { dg-final { scan-assembler-not {\mbne} } } */
