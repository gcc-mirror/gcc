/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mbmi -fno-inline -dp" } */

#include "bmi-andn-1.c"

/* { dg-final { scan-assembler-times "andndi" 1 } } */
