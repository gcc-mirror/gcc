// PR c++/121893
// { dg-additional-options "-fmodules" }

import M;
int main() {
  ::go(ns::S{});
}
