/*
TEST_OUTPUT:
---
fail_compilation/fail13120.d(13): Error: `pure` delegate `fail13120.g1.__foreachbody2` cannot call impure function `fail13120.f1`
fail_compilation/fail13120.d(13): Error: `@nogc` delegate `fail13120.g1.__foreachbody2` cannot call non-@nogc function `fail13120.f1`
---
*/
void f1() {}

void g1(char[] s) pure @nogc
{
    foreach (dchar dc; s)
        f1();
}

/*
TEST_OUTPUT:
---
fail_compilation/fail13120.d(35): Error: `pure` function `fail13120.h2` cannot call impure function `fail13120.g2!().g2`
fail_compilation/fail13120.d(35): Error: `@safe` function `fail13120.h2` cannot call `@system` function `fail13120.g2!().g2`
fail_compilation/fail13120.d(27):        `fail13120.g2!().g2` is declared here
fail_compilation/fail13120.d(35): Error: `@nogc` function `fail13120.h2` cannot call non-@nogc function `fail13120.g2!().g2`
---
*/
void f2() {}

void g2()(char[] s)
{
    foreach (dchar dc; s)
        f2();
}

void h2() @safe pure @nogc
{
    g2(null);
}
