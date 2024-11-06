/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-require-effective-target arm32 } */
/* { dg-require-effective-target arm_cpu_cortex_a57_ok } */
/* { dg-options "-O2 -save-temps" } */
/* { dg-add-options arm_cpu_cortex_a57 } */

#include "../aarch64/mod_2.x"

/* { dg-final { scan-assembler "rsblt\tr\[0-9\]*" } } */
/* { dg-final { scan-assembler-times "and\tr\[0-9\].*1" 1 } } */
