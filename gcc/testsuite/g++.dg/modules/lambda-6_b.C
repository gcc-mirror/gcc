// PR c++/107398
// { dg-additional-options "-fmodules-ts" }

import Lambda6;

int main() {
  if (foo() != 1)
    __builtin_abort();
}
