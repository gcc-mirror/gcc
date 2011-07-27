/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fno-inline -dp" } */

#include "bmi-andn-2.c"

/* { dg-final { scan-assembler-times "bmi_andn_si" 1 } } */
