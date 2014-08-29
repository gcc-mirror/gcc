// PR c++/54002
// { dg-do compile { target c++11 } }

class C1 {
  constexpr static int foo(int x) { return x + 1; }
  constexpr static int bar = foo(sizeof(int)); // { dg-error "constant expression" }
};
