// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-msse" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include <xmmintrin.h>
