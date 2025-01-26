/* { dg-do compile } */
/* { dg-options "-O2 -mmcu=atmega103" } */

#include "pr118012-1.h"

/* { dg-final { scan-assembler-not "mul" } } */
/* { dg-final { scan-assembler-not "neg" } } */
