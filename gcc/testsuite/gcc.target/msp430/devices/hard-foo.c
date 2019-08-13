/* { dg-do compile } */
/* { dg-options "-mmcu=msp430foo" } */
/* { dg-warning "Unrecognized MCU name 'msp430foo'.*\n.*Use the" "" { target *-*-* } 0 } */

#include "../devices-main.c"
