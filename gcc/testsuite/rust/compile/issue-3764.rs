#![feature(no_core)]
#![no_core]

struct Foo<T = U, U = ()>(T, U);
// { dg-error "type parameters with a default cannot use forward declared identifiers" "" { target *-*-* } .-1 }

struct Bar<T, U = T>(T, U);
