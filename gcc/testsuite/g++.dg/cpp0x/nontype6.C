// PR c++/113266, PR c++/116911
// { dg-do compile }

template <int &> struct a {};
static int guard1;
a<guard1> b;  // { dg-error "constant-expression|invalid" "" { target c++98_only } }

namespace {
  int guard2;
}
a<guard2> c;  // OK in C++98 because guard2 has external linkage
              // OK since C++11 because we can refer to an internal linkage decl

void nolinkage() {
  static int guard3;
  a<guard3> d;  // { dg-error "constant-expression|invalid" "" { target c++98_only } }
  // { dg-error "constant expression|no linkage" "" { target { c++11 && c++14_down } } .-1 }
  // OK since C++17 since we can now refer to no-linkage decls
}
