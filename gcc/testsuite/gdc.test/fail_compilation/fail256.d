/*
TEST_OUTPUT:
---
fail_compilation/fail256.d(8): Error: incompatible types for (("foo"d) ~ ("bar"c)): 'dstring' and 'string'
---
*/

auto s = "foo"d ~ "bar"c;
