/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options "-std=gnu99" } */

typedef __UINT32_TYPE__ TYP;
#define AS __flashx

#include "pr119989.h"
