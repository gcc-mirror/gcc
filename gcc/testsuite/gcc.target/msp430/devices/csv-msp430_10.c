/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_10" } */
/* { dg-warning "does not have hardware multiply" "" { target msp430_hwmul_not_none } 0 } */
/* { dg-warning "supports 430X ISA but '-mcpu' option is set to 430" "" { target msp430_430_selected } 0 } */

#include "devices-main.c"
