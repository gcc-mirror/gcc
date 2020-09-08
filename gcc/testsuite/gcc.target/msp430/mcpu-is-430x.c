/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" "-mmcu=*" } { "" } } */
/* { dg-options "-mcpu=430x" } */

/* Verify that the alternate way of selecting the 430X ISA (i.e. with the
   value "430x" instead of "msp430x") successfully selects the correct
   ISA.  */

#ifndef __MSP430X__
#error
#endif

