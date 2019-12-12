/* { dg-do compile } */
/* { dg-skip-if "MCU supports 430 ISA only" { *-*-* } { "-mlarge" "-mcpu=msp430x*" } { "" } } */
/* { dg-additional-options "-mmcu=msp430f012 -mcpu=msp430 -mhwmult=16bit" } */

/* Test that MCU names in devices.csv are only chosen if the full device name
   is matched exactly.
   msp430f0123 (with 430X ISA and f5series hwmult) appears before msp430f012 in
   devices.csv, but should not be matched.
   Errors and warnings will be emitted if msp430f0123 is wrongly matched.  */

#include "devices-main.c"
