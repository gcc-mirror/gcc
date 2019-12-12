/*
TEST_OUTPUT:
---
fail_compilation/fail291.d(9): Error: variable fail291.X cannot be declared to be a function
---
*/

auto a() { return 0; }
typeof(a) X;
