/*
TEST_OUTPUT:
---
fail_compilation/fail198.d(8): Error: template instance `test!42` template `test` is not defined
---
*/

int x = test!(42);
