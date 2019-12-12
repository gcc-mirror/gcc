/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430fr5969" } */
/* MSP430FR5969 has msp430x ISA and f5series hwmult in the hard-coded data,
   check that the different values for this device in devices.csv override it.
   */
/* { dg-warning "does not have hardware multiply" "" { target msp430_hwmul_not_none } 0 } */
/* { dg-warning "supports 430 ISA but" "" { target msp430_430x_selected } 0 } */
/* { dg-error "'-mlarge' requires a 430X-compatible '-mmcu='" "" { target msp430_mlarge_selected } 0 } */


#include "devices-main.c"
