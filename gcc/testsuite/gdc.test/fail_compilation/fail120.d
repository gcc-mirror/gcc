/*
TEST_OUTPUT:
---
fail_compilation/fail120.d(12): Error: accessing non-static variable `nodes` requires an instance of `Foo`
fail_compilation/fail120.d(13): Error: accessing non-static variable `nodes` requires an instance of `Foo`
---
*/

class Foo
{
    int[2] nodes;
    auto left = (){ return nodes[0]; };
    auto right = (){ return nodes[1]; };
}
