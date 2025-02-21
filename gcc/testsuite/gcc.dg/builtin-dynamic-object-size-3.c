/* { dg-do run } */
/* { dg-options "-O2 -Wno-stringop-overread" } */
/* { dg-additional-options "-DSKIP_STRNDUP" { target { ! strndup } } } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-3.c"
