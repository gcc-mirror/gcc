/* { dg-do compile } */
/* { dg-options "-mmcu=cc430f5123" } */
/* { dg-warning "supports 430X ISA but '-mcpu' option is set to 430" "" { target msp430_430_selected } 0 } */
/* { dg-warning "supports 32-bit .5xx. hardware multiply" "" { target msp430_hwmul_not_f5 } 0 } */

/* revision=2, hwmpy=8 */
#include "devices-main.c"
