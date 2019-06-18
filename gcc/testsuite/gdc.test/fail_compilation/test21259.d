// https://issues.dlang.org/show_bug.cgi?id=21259
// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/test21259.d(39): Deprecation: alias `test21259.Foo.width` is deprecated
fail_compilation/test21259.d(40): Deprecation: alias `test21259.Foo2.width` is deprecated
fail_compilation/test21259.d(41): Deprecation: variable `test21259.Foo3.bar` is deprecated
fail_compilation/test21259.d(42): Deprecation: alias `test21259.Foo4.width` is deprecated
---
*/

struct Foo
{
    int bar;
    deprecated alias width = bar;
}

struct Foo2
{
    deprecated int bar;
    deprecated alias width = bar;
}

struct Foo3
{
    deprecated int bar;
}

struct Foo4
{
    int bar;
deprecated:
    alias width = bar;
}

void main()
{
    Foo a = { width : 100};
    Foo2 b = { width : 100};
    Foo3 c = { bar : 100};
    Foo4 d = { width : 100};
}

// deprecations inside a deprecated scope shouldn't be triggered
deprecated void test()
{
    Foo a = { width : 100};
    Foo2 b = { width : 100};
    Foo3 c = { bar : 100};
    Foo4 d = { width : 100};
}
