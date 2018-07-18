// PR c++/69628
// { dg-do compile }

0''; // { dg-error "empty character constant" }
// { dg-error "expected unqualified-id before numeric constant" "" { target *-*-* } .-1 }
