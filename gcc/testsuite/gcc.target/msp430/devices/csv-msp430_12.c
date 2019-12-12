/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_12" } */
/* { dg-warning "supports 430X ISA but '-mcpu' option is set to 430" "" { target msp430_430_selected } 0 } */
/* { dg-warning "supports 16-bit hardware multiply" "" { target msp430_hwmul_not_16bit } 0 } */

#include "devices-main.c"
