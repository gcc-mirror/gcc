// PR c++/84636

typedef void a();
struct A {
a: 1;  // { dg-error "bit-field .\\<anonymous\\>. with non-integral type" }
};
