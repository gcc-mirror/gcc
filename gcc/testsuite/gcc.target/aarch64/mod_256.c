/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a57 -save-temps" } */

#include "mod_256.x"

/* { dg-final { scan-assembler "csneg\t\[wx\]\[0-9\]*" } } */
