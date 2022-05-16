// https://issues.dlang.org/show_bug.cgi?id=21092

/*
TEST_OUTPUT:
---
fail_compilation/fail21092.d(19): Error: using the result of a comma expression is not allowed
fail_compilation/fail21092.d(19): Error: using `*` on an array is no longer supported; use `*(T , U).ptr` instead
fail_compilation/fail21092.d(19): Error: `*(T , cast(real*)U)` has no effect
fail_compilation/fail21092.d(26): Error: using the result of a comma expression is not allowed
fail_compilation/fail21092.d(26): Error: using `*` on an array is no longer supported; use `*(w , SmallStirlingCoeffs).ptr` instead
fail_compilation/fail21092.d(26): Error: `*(w , cast(real*)SmallStirlingCoeffs)` has no effect
---
*/

real[] T;
real[] U = [];
real erf()
{
    *(T, U);
}

real gammaStirling()
{
    static real[] SmallStirlingCoeffs = [];
    real w;
    *(w, SmallStirlingCoeffs);
}
