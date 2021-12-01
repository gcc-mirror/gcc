/*
EXTRA_FILES: imports/fail1900a.d imports/fail1900b.d
TEST_OUTPUT:
---
fail_compilation/fail1900.d(27): Error: template `fail1900.Mix1a!().Foo` matches more than one template declaration:
fail_compilation/fail1900.d(14):     `Foo(ubyte x)`
and
fail_compilation/fail1900.d(15):     `Foo(byte x)`
---
*/

template Mix1a()
{
    template Foo(ubyte x) {}
    template Foo(byte x) {}
}
template Mix1b()
{
    template Foo(int x) {}
}

mixin Mix1a;
mixin Mix1b;

void test1900a()
{
    alias x = Foo!1;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail1900.d(42): Error: template `imports.fail1900b.Bar(short n)` at fail_compilation/imports/fail1900b.d(2) conflicts with template `imports.fail1900a.Bar(int n)` at fail_compilation/imports/fail1900a.d(2)
---
*/

import imports.fail1900a;
import imports.fail1900b;

void test1900b()
{
    enum x = Bar!1;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail1900.d(66): Error: template `fail1900.Mix2b!().Baz(int x)` at fail_compilation/fail1900.d(58) conflicts with template `fail1900.Mix2a!().Baz(byte x)` at fail_compilation/fail1900.d(54)
---
*/

template Mix2a()
{
    template Baz(byte x) {}
}
template Mix2b()
{
    template Baz(int x) {}
}

mixin Mix2a;
mixin Mix2b;

void test1900c()
{
    alias x = Baz!1;
}
