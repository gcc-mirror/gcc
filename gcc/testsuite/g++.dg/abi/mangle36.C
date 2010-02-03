// PR c++/41959
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-mavx -fabi-version=4" }
// { dg-final { scan-assembler "_Z1fDv4_f" } }
// { dg-final { scan-assembler "_Z1fDv8_f" } }

#include <x86intrin.h>
void f(__m128) { }
void f(__m256) { }
