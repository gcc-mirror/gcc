/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -march=x86-64 -m128bit-atomic" } */
/* { dg-final { scan-assembler-times "lock;?\[ \\t\]+cmpxchg16b" 8 } } */

#include <x86intrin.h>
#include "pr126293-1a.c"
