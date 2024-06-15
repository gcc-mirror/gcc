// PR c++/115231
// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  // Check that deduction guides are reachable,
  // and that they declared the right type.
  A a(1);
  A<int> a2 = a;

  B b(2);
  B<double> b2 = b;

  C<int>::I x(10);
  C<int>::I<int> x2 = x;

  C<int>::I y("xyz");
  C<int>::I<char> y2 = y;
}
