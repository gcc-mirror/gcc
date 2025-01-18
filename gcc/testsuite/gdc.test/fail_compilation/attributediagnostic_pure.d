/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic_pure.d(19): Error: `pure` function `D main` cannot call impure function `attributediagnostic_pure.gc`
fail_compilation/attributediagnostic_pure.d(14):        and executing an `asm` statement without `pure` annotation makes it fail to infer `pure`
---
*/

// Issue 17374 - Improve inferred attribute error message
// https://issues.dlang.org/show_bug.cgi?id=17374

auto gc()
{
    asm {}
}

void main() pure
{
    gc();
}
