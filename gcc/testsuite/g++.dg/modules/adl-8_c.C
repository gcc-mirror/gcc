// { dg-additional-options "-fmodules -std=c++26" }

import X;

int main() {
  test1();
  test2(ns::S{});
  test3(other::S{});
}
