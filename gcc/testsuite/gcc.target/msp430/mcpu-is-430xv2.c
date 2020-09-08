/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=*" "-mmcu=*" } { "" } } */
/* { dg-options "-mcpu=430XV2" } */

/* Verify that the alternate way of selecting the 430XV2 ISA (i.e. with the
   value "430XV2" instead of "msp430xv2") successfully selects the correct
   ISA.  430xv2 doesn't actually have any observable effect on codegen, so we
   have to just test for 430X.  */

#ifndef __MSP430X__
#error
#endif

