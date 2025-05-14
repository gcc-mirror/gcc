/*
PERMUTE_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test21912.d(28): Error: function `test21912.escapeParam` is `@nogc` yet allocates closure for `escapeParam()` with the GC
fail_compilation/test21912.d(30):        delegate `() => i` closes over variable `i`
fail_compilation/test21912.d(28):        `i` declared here
fail_compilation/test21912.d(33): Error: function `test21912.escapeAssign` is `@nogc` yet allocates closure for `escapeAssign()` with the GC
fail_compilation/test21912.d(35):        delegate `() => i` closes over variable `i`
fail_compilation/test21912.d(33):        `i` declared here
fail_compilation/test21912.d(44): Error: function `test21912.escapeAssignRef` is `@nogc` yet allocates closure for `escapeAssignRef()` with the GC
fail_compilation/test21912.d(46):        delegate `() => i` closes over variable `i`
fail_compilation/test21912.d(44):        `i` declared here
fail_compilation/test21912.d(55): Error: function `test21912.escapeParamInferred` is `@nogc` yet allocates closure for `escapeParamInferred()` with the GC
fail_compilation/test21912.d(57):        delegate `() => i` closes over variable `i`
fail_compilation/test21912.d(55):        `i` declared here
---
*/
@nogc:

alias Dg = @nogc int delegate();

Dg identity(return scope Dg dg)
{
    return dg;
}

Dg escapeParam(int i)
{
    return identity(() => i);
}

Dg escapeAssign(int i, return scope Dg dg)
{
    dg = () => i;
    return dg;
}

ref Dg identityR(return ref scope Dg dg)
{
    return dg;
}

ref Dg escapeAssignRef(int i, return ref scope Dg dg)
{
    dg = () => i;
    return dg;
}

auto identityInferred(Dg dg)
{
    return dg;
}

Dg escapeParamInferred(int i)
{
    return identityInferred(() => i);
}
