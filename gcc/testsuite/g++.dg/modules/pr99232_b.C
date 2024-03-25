// PR c++/99232
// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

import pr99232;

double foo() { return lambda * 2.0; }
static_assert(a == 42);

int main() {
  if (&lambda != get_lambda_addr())
    __builtin_abort();
}
