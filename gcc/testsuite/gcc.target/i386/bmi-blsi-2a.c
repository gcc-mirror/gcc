/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fno-inline -dp" } */

#include "bmi-blsi-2.c"

/* { dg-final { scan-assembler-times "bmi_blsi_si" 1 } } */
