/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic_nogc.d(21): Error: `@nogc` function `attributediagnostic_nogc.layer2` cannot call non-@nogc function `attributediagnostic_nogc.layer1`
fail_compilation/attributediagnostic_nogc.d(22):        which calls `attributediagnostic_nogc.layer0`
fail_compilation/attributediagnostic_nogc.d(23):        which calls `attributediagnostic_nogc.gc`
fail_compilation/attributediagnostic_nogc.d(27):        which wasn't inferred `@nogc` because of:
fail_compilation/attributediagnostic_nogc.d(27):        `asm` statement in function `attributediagnostic_nogc.gc` is assumed to use the GC - mark it with `@nogc` if it does not
fail_compilation/attributediagnostic_nogc.d(43): Error: `@nogc` function `D main` cannot call non-@nogc function `attributediagnostic_nogc.gc1`
fail_compilation/attributediagnostic_nogc.d(32):        which wasn't inferred `@nogc` because of:
fail_compilation/attributediagnostic_nogc.d(32):        cannot use `new` in `@nogc` function `attributediagnostic_nogc.gc1`
fail_compilation/attributediagnostic_nogc.d(44): Error: `@nogc` function `D main` cannot call non-@nogc function `attributediagnostic_nogc.gc2`
fail_compilation/attributediagnostic_nogc.d(38):        which wasn't inferred `@nogc` because of:
fail_compilation/attributediagnostic_nogc.d(38):        `@nogc` function `attributediagnostic_nogc.gc2` cannot call non-@nogc `fgc`
fail_compilation/attributediagnostic_nogc.d(45): Error: `@nogc` function `D main` cannot call non-@nogc function `attributediagnostic_nogc.gcClosure`
fail_compilation/attributediagnostic_nogc.d(48):        which wasn't inferred `@nogc` because of:
fail_compilation/attributediagnostic_nogc.d(48):        function `attributediagnostic_nogc.gcClosure` is `@nogc` yet allocates closure for `gcClosure()` with the GC
---
*/
#line 18
// Issue 17374 - Improve inferred attribute error message
// https://issues.dlang.org/show_bug.cgi?id=17374

auto layer2() @nogc { layer1(); }
auto layer1() { layer0(); }
auto layer0() { gc(); }

auto gc()
{
    asm {}
}

auto gc1()
{
    int* x = new int;
}

auto fgc = function void() {new int[10];};
auto gc2()
{
    fgc();
}

void main() @nogc
{
    gc1();
    gc2();
    gcClosure();
}

auto gcClosure()
{
    int x;
    int bar() { return x; }
    return &bar;
}
