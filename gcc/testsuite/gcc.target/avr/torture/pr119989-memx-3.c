/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99" } */

__extension__ typedef __uint24 TYP;
#define AS __memx

#include "pr119989.h"
