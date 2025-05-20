// PR c++/120040
// { dg-additional-options "-fmodules -std=c++20" }

import M;

constexpr bool qux() {
  auto* s = bar();
  delete[] s;
  return true;
}

int main() {
  static_assert(foo());
  static_assert(qux());
}
