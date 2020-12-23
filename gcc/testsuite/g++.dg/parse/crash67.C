// PR c++/79414
// { dg-additional-options "-Wno-return-type" }

class x0;
template <x1> x2() {  // { dg-error "declared|type" }
x0 x3 = x3. // { dg-error "incomplete type" }
// { dg-error "12:expected" "" { target *-*-* } .-1 }
