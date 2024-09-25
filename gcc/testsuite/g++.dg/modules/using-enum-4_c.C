// PR c++/114683
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules-ts" }

import M;
int main() {
  auto y = bar::y;

  // This should not be made visible.
  auto z = foo::a::z;  // { dg-error "not been declared" }
}
