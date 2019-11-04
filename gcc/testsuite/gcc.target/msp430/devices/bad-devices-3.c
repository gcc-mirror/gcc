/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_00 -mno-warn-mcu" } */
/* { dg-warning "'CPU_TYPE' and 'MPY_TYPE' column headings are missing from 'devices.csv'" "" { target *-*-* } 0 } */

#include "devices-main.c"
