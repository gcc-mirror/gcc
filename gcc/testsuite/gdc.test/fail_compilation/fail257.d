/*
TEST_OUTPUT:
---
fail_compilation/fail257.d(8): Error: incompatible types for `("foo"d) == ("bar"c)`: `dstring` and `string`
fail_compilation/fail257.d(8):        while evaluating `pragma(msg, "foo"d == "bar"c ? "A" : "B")`
---
*/
pragma(msg, "foo"d == "bar"c ? "A" : "B");
