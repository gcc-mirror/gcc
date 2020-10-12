/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" "-mmcu=*" } } */
/* { dg-options "-mmcu=msp430foobar -mno-warn-mcu -mno-warn-devices-csv" } */

/* Verify that the default ISA is set to 430X when the MCU passed to -mmcu= is
   unrecognized.  */

#ifndef __MSP430X__
#error
#endif
