/* { dg-do run } */
/* { dg-require-effective-target sync_int_long } */

#define basetype _Atomic int

#define NO_BITFIELDS 1

#include "hardbool.c"
