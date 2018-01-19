/* { dg-options "-std=gnu99 -Tavr51-flash1.x" } */
/* { dg-do run { target { ! avr_tiny } } } */

#define __as __flash1

#include "addr-space-2.h"
