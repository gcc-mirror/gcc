// PR c++/84942
// { dg-do compile { target c++14 } }
// { dg-options "-w" }

int a(__attribute__((b((int)__builtin_inf() * 1ULL / auto))));
// { dg-error "expected primary-expression before" "" { target *-*-* } .-1 }
