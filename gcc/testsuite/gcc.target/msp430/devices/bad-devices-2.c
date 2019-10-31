/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_00 -mno-warn-mcu" } */
/* { dg-warning "'MPY_TYPE' column heading is missing from 'devices.csv'" "" { target *-*-* } 0 } */

#include "devices-main.c"
