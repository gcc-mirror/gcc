// PR c++/41959
// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-mavx -fabi-version=2" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include <x86intrin.h>
void f(__m128) { }	// { dg-message "previous mangling" }
void f(__m256) { }	// { dg-error "conflicts with a previous mangle" }
// { dg-message "mangling" "" { target *-*-* } .-1 }
