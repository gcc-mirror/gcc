/* { dg-options "-std=gnu99 -Wa,--no-warn" } */
/* { dg-do run { target { ! avr_tiny } } } */

/* --no-warn because: "assembling 24-bit address needs binutils extension"
   see binutils PR13503.  */

#define __as __memx

#include "addr-space-2.h"
