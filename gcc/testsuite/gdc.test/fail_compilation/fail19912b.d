// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/fail19912b.d(8): Error: class `fail19912b.object` conflicts with import `fail19912b.object` at fail_compilation/fail19912b.d
---
*/
class object { }
void fun(string) { }
