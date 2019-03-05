/*
TEST_OUTPUT:
---
fail_compilation/ice13987.d(9): Error: cannot use array to initialize S
---
*/

struct S {}
S s = [{}];
