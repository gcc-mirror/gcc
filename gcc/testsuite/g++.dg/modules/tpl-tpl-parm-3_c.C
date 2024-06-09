// PR c++/98881
// { dg-additional-options "-fmodules-ts" }

import "tpl-tpl-parm-3_a.H";

template <typename T> struct A {};
template <typename T> struct B {};

void foo() {
  X<A<int>> a;
  X<B<int>> b;
  a.f(b);

  Y<A> y;
}
