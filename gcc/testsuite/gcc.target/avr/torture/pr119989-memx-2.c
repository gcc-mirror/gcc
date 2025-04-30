/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99" } */

typedef __UINT16_TYPE__ TYP;
#define AS __memx

#include "pr119989.h"
