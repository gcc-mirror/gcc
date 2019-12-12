/* { dg-do compile } */
/* { dg-additional-options "-mmcu=msp430_00 -mno-warn-mcu" } */
/* { dg-warning "'CPU_TYPE' column heading is missing from 'devices.csv'" "" { target *-*-* } 0 } */

#include "devices-main.c"
