/*
TEST_OUTPUT:
---
fail_compilation/fail_contracts1.d(8): Error: `(identifier) { ... }` or `(identifier; expression)` following `out` expected, not `)`
---
*/

void foo() out()){}
