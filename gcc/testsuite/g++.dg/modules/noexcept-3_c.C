// PR c++/119462
// { dg-additional-options "-fmodules -std=c++20" }

import M;
int main() {
  enqueue();

  constexpr exception_ptr e;
  static_assert(e == e);
  static_assert(noexcept(e == e));
}
