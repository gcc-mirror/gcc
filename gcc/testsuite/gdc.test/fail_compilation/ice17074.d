/*
*/
extern(C++, std.__overloadset) void ice_std_keyword();

/*
TEST_OUTPUT:
---
fail_compilation/ice17074.d(13): Error: identifier expected for C++ namespace
fail_compilation/ice17074.d(13): Error: found `*` when expecting `)`
fail_compilation/ice17074.d(13): Error: declaration expected, not `)`
---
*/
extern(C++, std.*) void ice_std_token();
