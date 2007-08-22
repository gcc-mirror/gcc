/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#define TYPE unsigned int
#define POPCNT _mm_popcnt_u32

#include "sse4_2-popcnt.h"
