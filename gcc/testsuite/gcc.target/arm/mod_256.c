/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_cpu_cortex_a57 } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_cpu_cortex_a57 } */

#include "../aarch64/mod_256.x"

/* { dg-final { scan-assembler "rsbpl\tr\[0-9\]*" } } */

