/*
TEST_OUTPUT:
---
fail_compilation/ice11626.d(8): Error: undefined identifier `Bar`
---
*/

void foo(const ref Bar) {}
