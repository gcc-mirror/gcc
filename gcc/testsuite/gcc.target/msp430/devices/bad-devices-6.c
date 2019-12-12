/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_00 -mno-warn-mcu" } */
/* { dg-warning "invalid 'MPY_TYPE' value of '3' read from 'devices.csv' for 'msp430_00'" "" { target *-*-* } 0 } */

#include "devices-main.c"
