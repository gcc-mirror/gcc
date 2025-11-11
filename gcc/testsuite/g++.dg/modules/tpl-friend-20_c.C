// PR c++/122551
// { dg-additional-options "-fmodules" }

import X;

int main() {
  f(test());
  g(test());
  h(test());
  i(test());
}
