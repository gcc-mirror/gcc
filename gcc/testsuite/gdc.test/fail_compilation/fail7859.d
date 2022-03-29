/*
TEST_OUTPUT:
---
fail_compilation/fail7859.d(9): Error: undefined identifier `NonExistent`
---
*/
template A(alias B) {}

mixin template C(alias B = cast(NonExistent)null) {
  alias A!B D;
}

mixin C!();
