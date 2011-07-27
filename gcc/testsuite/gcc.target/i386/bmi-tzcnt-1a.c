/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mbmi -fno-inline" } */

#include "bmi-tzcnt-1.c"

/* { dg-final { scan-assembler-times "tzcntq" 1 } } */
