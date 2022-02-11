/*
REQUIRED_ARGS: -o-
TEST_OUTPUT:
----
$p:druntime/import/core/internal/array/operations.d$($n$): Error: static assert:  "Binary op `*=` not supported for types `int*` and `int*`."
$p:druntime/import/core/internal/array/operations.d$($n$):        instantiated from here: `typeCheck!(true, int*, int*, "*=")`
$p:druntime/import/object.d$($n$):        instantiated from here: `arrayOp!(int*[], int*[], "*=")`
fail_compilation/fail_arrayop3c.d(15):        instantiated from here: `_arrayOp!(int*[], int*[], "*=")`
----
*/
void test11376()
{
    int*[] pa1;
    int*[] pa2;
    pa1[] *= pa2[];
}
