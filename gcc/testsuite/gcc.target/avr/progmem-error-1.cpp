/* { dg-do compile } */

#include "progmem.h"

char str[] PROGMEM = "Hallo";
/* This is the line number of the PROGMEM definition in progmem.h.  Keep it 
   absolute.  */
/* { dg-error "must be const" "" { target avr-*-* } 1 } */
