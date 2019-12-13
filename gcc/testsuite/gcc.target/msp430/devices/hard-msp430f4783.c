/* { dg-do compile } */
/* { dg-options "-mmcu=msp430f4783" } */
/* { dg-warning "supports 32-bit hardware multiply" "" { target msp430_hwmul_not_32bit } 0 } */
/* { dg-warning "supports 430 ISA but" "" { target msp430_430x_selected } 0 } */
/* { dg-error "'-mlarge' requires a 430X-compatible '-mmcu='" "" { target msp430_mlarge_selected } 0 } */

/* revision=0, hwmpy=4  */
#include "devices-main.c"
