// PR c++/119938
// { dg-additional-options "-fmodules -std=c++20" }

import "tpl-nttp-2_a.H";

int main() {
  C c;
  E();
  static_assert(G() == 3);
}
