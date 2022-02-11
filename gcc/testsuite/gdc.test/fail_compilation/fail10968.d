/*
TEST_OUTPUT:
---
fail_compilation/fail10968.d(41): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(41): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(29):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(42): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(42): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(29):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(43): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(43): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(29):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(46): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(46): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(29):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(47): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(47): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(29):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(47): Error: `pure` function `fail10968.bar` cannot call impure function `core.internal.array.construction._d_arraysetctor!(SA[], SA)._d_arraysetctor`
fail_compilation/fail10968.d(48): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(48): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(29):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(48): Error: `pure` function `fail10968.bar` cannot call impure function `core.internal.array.construction._d_arrayctor!(SA[], SA)._d_arrayctor`
---
*/

struct SA
{
    this(this)
    {
        throw new Exception("BOOM!");
    }
}

void bar() pure @safe
{
    SA    ss;
    SA[1] sa;

    // TOKassign
    ss = ss;
    sa = ss;
    sa = sa;

    // TOKconstruct
    SA    ss2 = ss;
    SA[1] sa2 = ss;
    SA[1] sa3 = sa;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail10968.d(74): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(75): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(76): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(79): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(80): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(81): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
---
*/

struct SD
{
    this(this) @disable;
}

void baz()
{
    SD    ss;
    SD[1] sa;

    // TOKassign
    ss = ss;
    sa = ss;
    sa = sa;

    // TOKconstruct
    SD    ss2 = ss;
    SD[1] sa2 = ss;
    SD[1] sa3 = sa;
}
