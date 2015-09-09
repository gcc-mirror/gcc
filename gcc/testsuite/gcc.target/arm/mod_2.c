/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

#include "../aarch64/mod_2.x"

/* { dg-final { scan-assembler "rsblt\tr\[0-9\]*" } } */
/* { dg-final { scan-assembler-times "and\tr\[0-9\].*1" 1 } } */
