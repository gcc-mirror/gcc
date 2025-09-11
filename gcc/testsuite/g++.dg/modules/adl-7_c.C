// PR c++/117658
// { dg-additional-options "-fmodules" }

import B;

int main() {
  ::test(ns1::S{});
  ::test(bar{});
}
