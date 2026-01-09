// PR c++/122609
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }

import M;

int main() {
  structured_binding<int>();
  operator_new<int>();
  use_typeid<int>();
  coroutine<int>();
}
