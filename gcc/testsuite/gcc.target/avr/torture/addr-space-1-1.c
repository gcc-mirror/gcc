/* { dg-options "-std=gnu99 -Tavr51-flash1.x" } */
/* { dg-do run { target { ! avr_tiny } } } */

#ifdef __FLASH1
#define __as __flash1
#else
#define __as __flash
#endif

#include "addr-space-1.h"
