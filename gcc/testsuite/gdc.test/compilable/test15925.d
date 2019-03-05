/* REQUIRED_ARGS: -transition=import -transition=checkimports
PERMUTE_ARGS:
TEST_OUTPUT:
---
compilable/test15925.d(17): Deprecation: local import search method found variable imp15925.X instead of nothing
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
