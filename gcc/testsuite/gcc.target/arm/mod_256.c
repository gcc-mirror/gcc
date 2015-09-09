/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

#include "../aarch64/mod_256.x"

/* { dg-final { scan-assembler "rsbpl\tr\[0-9\]*" } } */

