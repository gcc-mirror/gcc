/*
TEST_OUTPUT:
---
fail_compilation/fail15361.d(9): Error: unexpected `(` after `errorize`, inside `is` expression
fail_compilation/fail15361.d(9):        try enclosing the contents of `is` with a `typeof` expression
---
*/

enum isErrorizable(T) = is(errorize(T.init));
