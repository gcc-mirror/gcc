// PR c++/92134 - constinit malfunction in static data member.
// { dg-do compile { target c++20 } }

struct Value {
  Value() : v{new int{42}} {}
  int* v;
};

struct S {
  static constinit inline Value v{}; // { dg-error "variable .S::v. does not have a constant initializer|call to non-.constexpr. function" }
  // { dg-error "result of 'operator new'" "" { target implicit_constexpr } .-1 }
};

int main() { return *S::v.v; }
