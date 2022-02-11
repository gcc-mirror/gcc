// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/dephexstrings.d(8): Error: Built-in hex string literals are obsolete, use `std.conv.hexString!"60"` instead.
---
*/
enum xstr = x"60";

