/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fno-inline -dp" } */

#include "bmi-bextr-2.c"

/* { dg-final { scan-assembler-times "bmi_bextr_si" 1 } } */
