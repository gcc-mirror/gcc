// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

typename[: ^^:: :] x = 0;  // { dg-error "expected" }
[: ^^:: :] x2 = 0;  // { dg-error "expected" }
