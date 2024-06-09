// PR c++/104040
// { dg-additional-options "-fmodules-ts" }

import test;

int main() {
  test<bool> t{};
}
