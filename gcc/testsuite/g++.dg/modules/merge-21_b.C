// PR c++/124431
// { dg-additional-options "-fmodules" }
// { dg-module-cmi B }

export module B;
export import A;
struct X {
  mat<4> viewmatrix;
};

template struct S<0>;
