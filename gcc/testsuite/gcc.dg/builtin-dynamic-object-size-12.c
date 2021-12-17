/* { dg-do run } */
/* { dg-options "-O2" } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-12.c"
