/*
TEST_OUTPUT:
---
fail_compilation/fail19336.d(14): Error: template instance `Template!()` template `Template` is not defined
fail_compilation/fail19336.d(14): Error: circular reference to `fail19336.Foo.a`
fail_compilation/fail19336.d(17): Error: circular reference to `fail19336.b`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=19336

struct Foo
{
        Template!() a(a.x);
}

int b(b.x);
