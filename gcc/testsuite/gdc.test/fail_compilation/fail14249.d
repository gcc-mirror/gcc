/*
REQUIRED_ARGS: -unittest
TEST_OUTPUT:
---
fail_compilation/fail14249.d(21): Error: `shared static` constructor can only be member of module/aggregate/template, not function `main`
fail_compilation/fail14249.d(22): Error: `shared static` destructor can only be member of module/aggregate/template, not function `main`
fail_compilation/fail14249.d(23): Error: `static` constructor can only be member of module/aggregate/template, not function `main`
fail_compilation/fail14249.d(24): Error: `static` destructor can only be member of module/aggregate/template, not function `main`
fail_compilation/fail14249.d(25): Error: `unittest` can only be a member of module/aggregate/template, not function `main`
fail_compilation/fail14249.d(26): Error: `invariant` can only be a member of aggregate, not function `main`
fail_compilation/fail14249.d(27): Error: alias this can only be a member of aggregate, not function `main`
fail_compilation/fail14249.d(28): Error: constructor can only be a member of aggregate, not function `main`
fail_compilation/fail14249.d(29): Error: destructor can only be a member of aggregate, not function `main`
fail_compilation/fail14249.d(30): Error: postblit can only be a member of struct, not function `main`
fail_compilation/fail14249.d(31): Error: anonymous union can only be a part of an aggregate, not function `main`
fail_compilation/fail14249.d(35): Error: mixin `fail14249.main.Mix!()` error instantiating
---
*/
mixin template Mix()
{
    shared static this() {}
    shared static ~this() {}
    static this() {}    // from fail197.d, 1510 ICE: Assertion failure: 'ad' on line 925 in file 'func.c'
    static ~this() {}
    unittest {}
    invariant {}
    alias a this;
    this() {}           // from fail268.d
    ~this() {}          // from fail268.d
    this(this) {}
    union { int x; double y; }
}
void main()
{
    mixin Mix!();
}
