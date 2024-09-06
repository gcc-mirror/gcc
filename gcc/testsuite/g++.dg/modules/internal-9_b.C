// { dg-additional-options "-fmodules-ts" }

import "internal-9_a.H";

int main() {
  auto x2 = x;
  f();
  t<int>();

  auto y2 = y;
  g();
  u<int>();

  int val1 = ns::in_ns;

  A a;
  B<int> b;

  E e = X;
  F f = F::Y;

  U<int> temp;

#if __cplusplus >= 202002L
  static_assert(C<int>);
#endif

  int val2 = ns2::in_ns;
}
