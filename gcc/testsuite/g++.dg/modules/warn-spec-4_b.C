// PR c++/124459
// { dg-additional-options "-fmodules -Wunused-but-set-variable" }

import M;

void test() {
  foo(123);
}
