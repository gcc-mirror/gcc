/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mbmi -fno-inline -dp" } */

#include "bmi-blsmsk-1.c"

/* { dg-final { scan-assembler-times "bmi_blsmsk_di" 1 } } */
