/* { dg-do run { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } */
/* { dg-options "-O2 -DN=0x4000000" } */
/* { dg-additional-sources "builtin-dynamic-object-size-5-main.c" } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-5.c"
