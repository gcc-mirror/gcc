/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic_nothrow.d(21): Error: function `attributediagnostic_nothrow.layer1` is not `nothrow`
fail_compilation/attributediagnostic_nothrow.d(22):        which calls `attributediagnostic_nothrow.layer0`
fail_compilation/attributediagnostic_nothrow.d(23):        which calls `attributediagnostic_nothrow.gc`
fail_compilation/attributediagnostic_nothrow.d(27):        which wasn't inferred `nothrow` because of:
fail_compilation/attributediagnostic_nothrow.d(27):        `asm` statement is assumed to throw - mark it with `nothrow` if it does not
fail_compilation/attributediagnostic_nothrow.d(21): Error: function `attributediagnostic_nothrow.layer2` may throw but is marked as `nothrow`
fail_compilation/attributediagnostic_nothrow.d(43): Error: function `attributediagnostic_nothrow.gc1` is not `nothrow`
fail_compilation/attributediagnostic_nothrow.d(32):        which wasn't inferred `nothrow` because of:
fail_compilation/attributediagnostic_nothrow.d(32):        `object.Exception` is thrown but not caught
fail_compilation/attributediagnostic_nothrow.d(44): Error: function `attributediagnostic_nothrow.gc2` is not `nothrow`
fail_compilation/attributediagnostic_nothrow.d(41): Error: function `D main` may throw but is marked as `nothrow`
---
*/

// Issue 17374 - Improve inferred attribute error message
// https://issues.dlang.org/show_bug.cgi?id=17374

auto layer2() nothrow { layer1(); }
auto layer1() { layer0(); }
auto layer0() { gc(); }

auto gc()
{
    asm {}
}

auto gc1()
{
    throw new Exception("msg");
}

auto fgc = function void() {throw new Exception("msg");};
auto gc2()
{
    fgc();
}

void main() nothrow
{
    gc1();
    gc2();
}
