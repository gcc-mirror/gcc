// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct S {
  int &a;
  void foo (decltype(((a = 0) || ((auto)))));  // { dg-error "expected" }
};
