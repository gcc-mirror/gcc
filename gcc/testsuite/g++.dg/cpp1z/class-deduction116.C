// PR c++/106604
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Wunused-function" }

namespace {
  template<class T> struct A { A(...); };
  A(bool) -> A<bool>; // { dg-bogus "never defined" }
}
