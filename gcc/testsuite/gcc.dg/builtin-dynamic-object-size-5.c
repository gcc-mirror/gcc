/* { dg-do compile { target i?86-*-linux* i?86-*-gnu* x86_64-*-linux* } } */
/* { dg-options "-O2" } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-5.c"

/* { dg-final { scan-assembler-not "abort" } } */
