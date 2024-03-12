// https://issues.dlang.org/show_bug.cgi?id=22329
// EXTRA_FILES: imports/imp22329.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/imp22329.d(3): Error: no property `values` for type `test22329.Foo`
fail_compilation/test22329.d(13):        struct `Foo` defined here
fail_compilation/imports/imp22329.d(3): Error: incompatible types for `(arg) + (1)`: `Foo` and `int`
fail_compilation/test22329.d(21): Error: template instance `imp22329.func!(Foo)` error instantiating
---
*/

public struct Foo {
    private int values;
    alias values this;
}

void main()
{
    import imports.imp22329 : func;
    func(Foo());
}
