// 9892
/*
TEST_OUTPUT:
---
fail_compilation/fail9892.d(11): Error: enum member fail9892.a circular reference to enum member
---
*/

enum
{
    a = b, //Segfault!
    b
}
