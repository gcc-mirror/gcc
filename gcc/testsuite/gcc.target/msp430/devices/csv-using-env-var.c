/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_28" } */
/* { dg-warning "supports 430X ISA but '-mcpu' option is set to 430" "" { target msp430_430_selected } 0 } */
/* { dg-warning "supports f5series hardware multiply" "" { target msp430_hwmul_not_f5 } 0 } */

/* This tests that the environment variable MSP430_GCC_INCLUDE_DIR can be used
   to specify the path to the directory containing devices.csv.
   The variable is set in msp430.exp.  */

#include "devices-main.c"
