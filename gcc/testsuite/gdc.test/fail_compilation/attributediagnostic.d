/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic.d(24): Error: `@safe` function `attributediagnostic.layer2` cannot call `@system` function `attributediagnostic.layer1`
fail_compilation/attributediagnostic.d(26):        which calls `attributediagnostic.layer0`
fail_compilation/attributediagnostic.d(28):        which calls `attributediagnostic.system`
fail_compilation/attributediagnostic.d(30):        which wasn't inferred `@safe` because of:
fail_compilation/attributediagnostic.d(30):        `asm` statement is assumed to be `@system` - mark it with `@trusted` if it is not
fail_compilation/attributediagnostic.d(25):        `attributediagnostic.layer1` is declared here
fail_compilation/attributediagnostic.d(46): Error: `@safe` function `D main` cannot call `@system` function `attributediagnostic.system1`
fail_compilation/attributediagnostic.d(35):        which wasn't inferred `@safe` because of:
fail_compilation/attributediagnostic.d(35):        cast from `uint` to `int*` not allowed in safe code
fail_compilation/attributediagnostic.d(33):        `attributediagnostic.system1` is declared here
fail_compilation/attributediagnostic.d(47): Error: `@safe` function `D main` cannot call `@system` function `attributediagnostic.system2`
fail_compilation/attributediagnostic.d(41):        which wasn't inferred `@safe` because of:
fail_compilation/attributediagnostic.d(41):        `@safe` function `system2` cannot call `@system` `fsys`
fail_compilation/attributediagnostic.d(39):        `attributediagnostic.system2` is declared here
---
*/

// Issue 17374 - Improve inferred attribute error message
// https://issues.dlang.org/show_bug.cgi?id=17374

auto layer2() @safe { layer1(); }
auto layer1() { layer0(); }
auto layer0() { system(); }

auto system()
{
    asm {}
}

auto system1()
{
    int* x = cast(int*) 0xDEADBEEF;
}

auto fsys = function void() @system {};
auto system2()
{
    fsys();
}

void main() @safe
{
    system1();
    system2();
}
