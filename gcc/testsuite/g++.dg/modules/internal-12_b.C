// PR c++/119551
// { dg-module-do run }
// { dg-additional-options "-fmodules" }

import M;

int main() {
  if (*a != 5)
    __builtin_abort();
  if (*b != 5)
    __builtin_abort();
  if (a != b)
    __builtin_abort();
}
