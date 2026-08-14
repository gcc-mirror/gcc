/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -march=x86-64 -mno-cx16 -m128bit-atomic" } */
/* { dg-final { scan-assembler-times "call\[ \\t\]+_?__atomic_fetch_" 4 } } */
/* { dg-final { scan-assembler-times "jmp\[ \\t\]+_?__atomic_fetch_" 4 } } */

#include "pr126293-1a.c"
