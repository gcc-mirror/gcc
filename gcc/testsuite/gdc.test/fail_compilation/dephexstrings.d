// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/dephexstrings.d(9): Error: semicolon expected following auto declaration, not `"60"`
fail_compilation/dephexstrings.d(9): Error: declaration expected, not `"60"`
---
*/
enum xstr = x"60";
