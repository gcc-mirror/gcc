// PR c++/27315
// { dg-do compile }

struct A;                  // { dg-message "forward declaration" }
template void A::foo<0>(); // { dg-error "before|incomplete" }
