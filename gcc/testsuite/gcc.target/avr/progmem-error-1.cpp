/* { dg-do compile } */

#include "progmem.h"

char str[] PROGMEM = "Hallo"; /* { dg-error "must be const" "" { target avr-*-* } 1 } */
