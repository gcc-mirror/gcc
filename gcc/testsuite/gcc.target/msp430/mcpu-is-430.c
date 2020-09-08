/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" "-mmcu=*" "-mlarge" } { "" } } */
/* { dg-options "-mcpu=430" } */

/* Verify that the alternate way of selecting the 430 ISA (i.e. with the
   value "430" instead of "msp430") successfully selects the correct ISA.  */

#ifdef __MSP430X__
#error
#endif
