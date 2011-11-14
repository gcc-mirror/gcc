// PR c++/41959
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-mavx -fabi-version=2" }

#include <x86intrin.h>
void f(__m128) { }		// { dg-message "previous declaration" }
void f(__m256) { }		// { dg-error "conflicts" }
// { dg-message "mangling" "" { target *-*-* } 7 }
