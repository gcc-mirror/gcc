/* REQUIRED_ARGS:
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/test15925.d(18): Error: undefined identifier `X`
fail_compilation/test15925.d(18):        while evaluating: `static assert(X == 1)`
---
*/

mixin template Import()
{
    import imports.imp15925;
}

class Foo
{
    mixin Import!();
    static assert(X == 1);
}
