// PR c++/92450 - ICE with invalid nested name specifier.

typedef int C2;
struct B1 {
  struct B2 {
  };
};

struct S6g {
  C2 : B1:B2; // { dg-error "" }
};
