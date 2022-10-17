/*
TEST_OUTPUT:
---
fail_compilation/diag13609b.d(10): Error: base classes are not allowed for `struct`, did you mean `;`?
fail_compilation/diag13609b.d(11): Error: basic type expected, not `End of File`
fail_compilation/diag13609b.d(11): Error: { } expected following `struct` declaration
---
*/

struct S :
