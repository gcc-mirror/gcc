/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.2" } */

#define TYPE unsigned long long
#define POPCNT _mm_popcnt_u64

#include "sse4_2-popcnt.h"
