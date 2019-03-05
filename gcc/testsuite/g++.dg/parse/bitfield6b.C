typedef void a();
struct A {
a a1: 1;  // { dg-error "3:bit-field .void A::a1\\(\\). with non-integral type .void \\(A::\\)\\(\\)." }
};
