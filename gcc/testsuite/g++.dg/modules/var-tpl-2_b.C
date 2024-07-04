// PR c++/114170
// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

import "var-tpl-2_a.H";

int main() {
  if (v<int> != 42)
    __builtin_abort();
}
