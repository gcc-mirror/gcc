/*
REQUIRED_ARGS: -o-
TEST_OUTPUT:
----
$p:druntime/import/core/internal/array/operations.d$($n$): Error: static assert:  "Binary op `+=` not supported for types `string` and `string`."
$p:druntime/import/core/internal/array/operations.d$($n$):        instantiated from here: `typeCheck!(true, string, string, "+=")`
$p:druntime/import/object.d$($n$):        instantiated from here: `arrayOp!(string[], string[], "+=")`
fail_compilation/fail_arrayop3b.d(15):        instantiated from here: `_arrayOp!(string[], string[], "+=")`
---
*/
void test11376()
{
    string[] s1;
    string[] s2;
    s2[] += s1[];
}
