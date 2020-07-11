// { dg-do compile { target c++20 } }

struct A {
  int operator==(const A&) const = default; // { dg-error "return .bool" }
  bool operator==(const A&, const A&) const = default; // { dg-error "exactly one" }
  bool operator==(int) const = default; // { dg-error "parameter type" }
  bool operator==(const A&) = default; // { dg-error "const" }
};
