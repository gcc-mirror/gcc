/*
TEST_OUTPUT:
---
fail_compilation/fail_contracts4.d(8): Error: missing `do { ... }` for function literal
---
*/

enum x = delegate int()in(true) out(;true) out(r; true) in{} out(r){};
