// PR c++/114889
// { dg-additional-options "-fmodules-ts" }

import "friend-8_a.H";

int main() {
  _Map_base<int, int> m;
  m.f();
}
