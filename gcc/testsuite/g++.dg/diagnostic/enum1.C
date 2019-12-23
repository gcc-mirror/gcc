// PR c++/92974 - bogus location for enum and non-enum in ?: warning.
// { dg-options "-Wextra" }

enum { X };

struct S { 
  template <typename T>
  void f(T) { unsigned int g(X ?: g); } // { dg-warning "enumerated and non-enumerated type in conditional expression" }
};

struct S2 { 
  S m;
  void l() { m.f(1); }
};
