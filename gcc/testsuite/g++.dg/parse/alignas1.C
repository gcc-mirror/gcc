// PR c++/64235
// { dg-do compile { target c++11 } }

struct S {
  double d;
};

struct alignas(sizeof(S) S1 { }; // { dg-error "expected '\\)' before 'S1'" }
struct alignas(16 S2 { }; // { dg-error "expected '\\)' before 'S2'" }
struct alignas(int S3 { }; // { dg-error "expected '\\)' before 'S3'" }
