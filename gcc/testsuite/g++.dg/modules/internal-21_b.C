// { dg-module-do run }
// { dg-additional-options "-fmodules" }

import M;

// Ideally M was not built and so this file is not needed at all,
// but while it is, let's at least check we function correctly.
int main() {
  if (x != 42)
    __builtin_abort ();
  if (y != 42)
    __builtin_abort ();
}
