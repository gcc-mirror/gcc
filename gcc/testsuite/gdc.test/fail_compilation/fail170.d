/*
TEST_OUTPUT:
---
fail_compilation/fail170.d(8): Error: variable fail170.foo.x cannot be final, perhaps you meant const?
---
*/

void foo(final out int x) { }
