/*
TEST_OUTPUT:
---
fail_compilation/fail9710.d(9): Error: static variable `e` cannot be read at compile time
---
*/

int* e;
enum v = e[1];
