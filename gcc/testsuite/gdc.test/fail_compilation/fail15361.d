/*
TEST_OUTPUT:
---
fail_compilation/fail15361.d(8): Error: unexpected `(` after `errorize`, inside `is` expression. Try enclosing the contents of `is` with a `typeof` expression
---
*/

enum isErrorizable(T) = is(errorize(T.init));
