/*
TEST_OUTPUT:
---
fail_compilation/fail10968.d(43): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(43): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(31):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(44): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(44): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(31):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(44): Error: `pure` function `fail10968.bar` cannot call impure function `core.internal.array.arrayassign._d_arraysetassign!(SA[], SA)._d_arraysetassign`
$p:druntime/import/core/internal/array/arrayassign.d$($n$):        which calls `core.lifetime.copyEmplace!(SA, SA).copyEmplace`
$p:druntime/import/core/lifetime.d$($n$):        which calls `fail10968.SA.__postblit`
fail_compilation/fail10968.d(45): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(45): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(31):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(45): Error: `pure` function `fail10968.bar` cannot call impure function `core.internal.array.arrayassign._d_arrayassign_l!(SA[], SA)._d_arrayassign_l`
$p:druntime/import/core/internal/array/arrayassign.d$-mixin-$n$($n$):        which calls `core.lifetime.copyEmplace!(SA, SA).copyEmplace`
$p:druntime/import/core/lifetime.d$($n$):        which calls `fail10968.SA.__postblit`
fail_compilation/fail10968.d(48): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(48): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(31):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(49): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(49): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(31):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(49): Error: `pure` function `fail10968.bar` cannot call impure function `core.internal.array.construction._d_arraysetctor!(SA[], SA)._d_arraysetctor`
$p:druntime/import/core/internal/array/construction.d$($n$):        which calls `core.lifetime.copyEmplace!(SA, SA).copyEmplace`
$p:druntime/import/core/lifetime.d$($n$):        which calls `fail10968.SA.__postblit`
fail_compilation/fail10968.d(50): Error: `pure` function `fail10968.bar` cannot call impure function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(50): Error: `@safe` function `fail10968.bar` cannot call `@system` function `fail10968.SA.__postblit`
fail_compilation/fail10968.d(31):        `fail10968.SA.__postblit` is declared here
fail_compilation/fail10968.d(50): Error: `pure` function `fail10968.bar` cannot call impure function `core.internal.array.construction._d_arrayctor!(SA[], SA)._d_arrayctor`
$p:druntime/import/core/internal/array/construction.d$($n$):        which calls `core.lifetime.copyEmplace!(SA, SA).copyEmplace`
$p:druntime/import/core/lifetime.d$($n$):        which calls `fail10968.SA.__postblit`
---
*/

#line 29
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
fail_compilation/fail10968.d(76): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(77): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(78): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(81): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(82): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
fail_compilation/fail10968.d(83): Error: struct `fail10968.SD` is not copyable because it has a disabled postblit
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
