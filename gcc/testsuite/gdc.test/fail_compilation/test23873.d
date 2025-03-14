// https://issues.dlang.org/show_bug.cgi?id=23873

/*
TEST_OUTPUT:
---
fail_compilation/imports/import23873.d(1): Error: (expression) expected following `static if`
fail_compilation/imports/import23873.d(1): Error: declaration expected following attribute, not `;`
fail_compilation/imports/import23873.d(3): Error: variable name expected after type `x`, not `End of File`
---
*/
struct Foo
{
    import imports.import23873;
}
