/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a" } */

#include "perm-luti.c"

/* { dg-final { scan-assembler-not {\tluti[246]\t} } } */
/* { dg-final { scan-assembler-times {\ttbl\t} 3 } } */
