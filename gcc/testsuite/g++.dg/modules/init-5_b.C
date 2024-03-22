// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  const int& x = A::x;
  if (x != -1)
    __builtin_abort();
}
