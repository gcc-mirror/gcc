/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

#define TYPE uint64_t
#define TYPE_MIN 0
#define TYPE_MAX UINT64_MAX
#define VALUE (uint64_t)-2

#include "max_plus_1.c"
