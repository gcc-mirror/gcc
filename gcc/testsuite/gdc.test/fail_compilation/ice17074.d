/*
TEST_OUTPUT:
---
fail_compilation/ice17074.d(9): Error: identifier expected for C++ namespace
fail_compilation/ice17074.d(9): Error: found `cast` when expecting `)`
fail_compilation/ice17074.d(9): Error: declaration expected, not `)`
---
*/
extern(C++, cast) void ice_keyword();

/*
TEST_OUTPUT:
---
fail_compilation/ice17074.d(19): Error: identifier expected for C++ namespace
fail_compilation/ice17074.d(19): Error: found `__overloadset` when expecting `)`
fail_compilation/ice17074.d(19): Error: declaration expected, not `)`
---
*/
extern(C++, std.__overloadset) void ice_std_keyword();

/*
TEST_OUTPUT:
---
fail_compilation/ice17074.d(29): Error: identifier expected for C++ namespace
fail_compilation/ice17074.d(29): Error: found `...` when expecting `)`
fail_compilation/ice17074.d(29): Error: declaration expected, not `)`
---
*/
extern(C++, ...) void ice_token();

/*
TEST_OUTPUT:
---
fail_compilation/ice17074.d(39): Error: identifier expected for C++ namespace
fail_compilation/ice17074.d(39): Error: found `*` when expecting `)`
fail_compilation/ice17074.d(39): Error: declaration expected, not `)`
---
*/
extern(C++, std.*) void ice_std_token();
