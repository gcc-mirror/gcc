/*
TEST_OUTPUT:
---
fail_compilation/fix21166.d(12): Error: invalid array operation `"foo" + "bar"` (possible missing [])
fail_compilation/fix21166.d(12):        did you mean to concatenate (`"foo" ~ "bar"`) instead ?
---
*/

// Test case for https://github.com/dlang/dmd/issues/21166
auto r =
	"foo"
	+
	"bar";
