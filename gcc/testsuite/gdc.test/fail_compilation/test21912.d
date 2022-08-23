/*
PERMUTE_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test21912.d(24): Error: function `test21912.escapeParam` is `@nogc` yet allocates closure for `escapeParam()` with the GC
fail_compilation/test21912.d(26):        `test21912.escapeParam.__lambda2` closes over variable `i` at fail_compilation/test21912.d(24)
fail_compilation/test21912.d(29): Error: function `test21912.escapeAssign` is `@nogc` yet allocates closure for `escapeAssign()` with the GC
fail_compilation/test21912.d(31):        `test21912.escapeAssign.__lambda3` closes over variable `i` at fail_compilation/test21912.d(29)
fail_compilation/test21912.d(40): Error: function `test21912.escapeAssignRef` is `@nogc` yet allocates closure for `escapeAssignRef()` with the GC
fail_compilation/test21912.d(42):        `test21912.escapeAssignRef.__lambda3` closes over variable `i` at fail_compilation/test21912.d(40)
fail_compilation/test21912.d(51): Error: function `test21912.escapeParamInferred` is `@nogc` yet allocates closure for `escapeParamInferred()` with the GC
fail_compilation/test21912.d(53):        `test21912.escapeParamInferred.__lambda2` closes over variable `i` at fail_compilation/test21912.d(51)
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
