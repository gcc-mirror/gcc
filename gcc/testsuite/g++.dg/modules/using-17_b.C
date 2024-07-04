// PR c++/114867
// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  int a = 0;
  ns::f(a);

  // Should also still find the inner::f overload
  auto e = get_e();
  int r = ns::f(e);
}
