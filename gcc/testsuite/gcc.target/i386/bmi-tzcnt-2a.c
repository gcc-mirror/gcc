/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fno-inline" } */

#include "bmi-tzcnt-2.c"

/* { dg-final { scan-assembler-times "tzcntl" 1 } } */
