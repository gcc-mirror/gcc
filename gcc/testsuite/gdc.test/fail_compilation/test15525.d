// https://issues.dlang.org/show_bug.cgi?id=15525

/*
TEST_OUTPUT:
---
fail_compilation/imports/import15525.d(3): Error: parenthesized template parameter list expected following template identifier
fail_compilation/imports/import15525.d(3): Error: (expression) expected following `static if`
fail_compilation/imports/import15525.d(3): Error: declaration expected, not `}`
fail_compilation/test15525.d(16): Error: template instance `Tuple!()` template `Tuple` is not defined
---
*/

struct CrashMe
{
    import imports.import15525;
    Tuple!() crash;
}
