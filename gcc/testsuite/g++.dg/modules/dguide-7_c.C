// PR c++/120023
// { dg-additional-options "-fmodules" }

import M.S;
import M.D;

template <> struct ns::S<int> { S(int) {} };

int main() {
  ns::S s(123);
  ns::S<int> s2 = s;
}
