/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_00 -mno-warn-mcu" } */
/* { dg-warning "invalid 'CPU_TYPE' value of '5' read from 'devices.csv' for 'msp430_00'" "" { target *-*-* } 0 } */

#include "devices-main.c"
