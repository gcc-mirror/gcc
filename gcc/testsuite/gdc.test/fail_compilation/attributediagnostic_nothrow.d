/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic_nothrow.d(19): Error: function `attributediagnostic_nothrow.layer1` is not `nothrow`
fail_compilation/attributediagnostic_nothrow.d(20):        which calls `attributediagnostic_nothrow.layer0`
fail_compilation/attributediagnostic_nothrow.d(21):        which calls `attributediagnostic_nothrow.gc`
fail_compilation/attributediagnostic_nothrow.d(25):        and executing an `asm` statement without a `nothrow` annotation makes it fail to infer `nothrow`
fail_compilation/attributediagnostic_nothrow.d(19): Error: function `attributediagnostic_nothrow.layer2` may throw but is marked as `nothrow`
fail_compilation/attributediagnostic_nothrow.d(41): Error: function `attributediagnostic_nothrow.gc1` is not `nothrow`
fail_compilation/attributediagnostic_nothrow.d(30):        and `object.Exception` being thrown but not caught makes it fail to infer `nothrow`
fail_compilation/attributediagnostic_nothrow.d(42): Error: function `attributediagnostic_nothrow.gc2` is not `nothrow`
fail_compilation/attributediagnostic_nothrow.d(39): Error: function `D main` may throw but is marked as `nothrow`
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
