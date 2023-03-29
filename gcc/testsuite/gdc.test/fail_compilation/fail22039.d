// https://issues.dlang.org/show_bug.cgi?id=22039

/*
TEST_OUTPUT:
---
fail_compilation/fail22039.d(11): Error: recursive evaluation of `func()`
fail_compilation/fail22039.d(14): Error: recursive evaluation of `gun(func2())`
---
*/

int func(int x = func()) { return x; }

int gun() { return 2; }
int func2(int x = gun(func2())) { return x; }
