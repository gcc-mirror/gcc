/*
TEST_OUTPUT:
---
fail_compilation/fail19912a.d(7): Error: struct `fail19912a.object` conflicts with import `fail19912a.object` at fail_compilation/fail19912a.d
---
*/
struct object { }
void fun(string) { }
