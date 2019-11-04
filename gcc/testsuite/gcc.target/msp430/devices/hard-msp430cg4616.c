/* { dg-do compile } */
/* { dg-options "-mmcu=msp430cg4616" } */
/* { dg-warning "supports 430X ISA but '-mcpu' option is set to 430" "" { target msp430_430_selected } 0 } */
/* { dg-warning "supports 16-bit hardware multiply" "" { target msp430_hwmul_not_16bit } 0 } */

/* revision=1, hwmpy=1 */
#include "devices-main.c"
