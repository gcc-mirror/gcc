/* { dg-do compile } */
/* { dg-options "-O2 -mmcu=atmega128" } */

#include "pr118360-1.h"

/* { dg-final { scan-assembler-not "mul" } } */
/* { dg-final { scan-assembler-not "neg" } } */
/* { dg-final { scan-assembler-not "rol " } } */
