/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -mbmi2" } */

#include "bmi2-mulx32-2.c"

/* { dg-final { scan-assembler-times "mulx\[ \\t\]+\[^\n\]*" 1 } } */
