// PR c++/116803
// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }

import "merge-18_b.H";

int main() {
  ns::foo<int>();
  static_assert(ns::bar<int> == 123);
}
