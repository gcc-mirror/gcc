/*
REQUIRED_ARGS: -o-
TEST_OUTPUT:
----
$p:druntime/import/core/internal/array/operations.d$($n$): Error: static assert:  "Binary `*` not supported for types `X` and `X`."
$p:druntime/import/core/internal/array/operations.d$($n$):        instantiated from here: `typeCheck!(true, X, X, X, "*", "=")`
$p:druntime/import/object.d$($n$):        instantiated from here: `arrayOp!(X[], X[], X[], "*", "=")`
fail_compilation/fail_arrayop3a.d(19):        instantiated from here: `_arrayOp!(X[], X[], X[], "*", "=")`
----
*/

void test11376()
{
    struct X { }

    auto x1 = [X()];
    auto x2 = [X()];
    auto x3 = [X()];
    x1[] = x2[] * x3[];

    string[] s1;
    string[] s2;
    s2[] += s1[];

    int*[] pa1;
    int*[] pa2;
    pa1[] *= pa2[];
}
