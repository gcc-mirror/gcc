/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_00 -mno-warn-mcu" } */
/* { dg-warning "format of column headings in 'devices.csv' is incorrect" "" { target *-*-* } 0 } */

#include "devices-main.c"
