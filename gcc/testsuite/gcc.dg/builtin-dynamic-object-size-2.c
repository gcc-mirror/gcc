/* { dg-do run } */
/* { dg-options "-O2 -Wno-stringop-overread" } */
/* { dg-require-effective-target alloca } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-2.c"
