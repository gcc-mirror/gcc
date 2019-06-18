/*
TEST_OUTPUT:
---
fail_compilation/ice9494.d(10): Error: circular reference to variable `ice9494.test`
fail_compilation/ice9494.d(14): Error: circular reference to variable `ice9494.Foo.test`
fail_compilation/ice9494.d(19): Error: circular reference to variable `ice9494.Bar.test`
---
*/

int[test] test;  // stack overflow

class Foo
{
    int[this.test] test;  // stack overflow
}

struct Bar
{
    int[this.test] test;  // stack overflow
}
