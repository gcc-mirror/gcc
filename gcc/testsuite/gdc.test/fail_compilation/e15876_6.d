/*
TEST_OUTPUT:
---
fail_compilation/e15876_6.d(7): Error: identifier expected following `immutable(int).`, not `;`
---
*/
auto unaryExParseError = immutable(int).;
