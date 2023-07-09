/*
TEST_OUTPUT:
---
fail_compilation/attributediagnostic_pure.d(20): Error: `pure` function `D main` cannot call impure function `attributediagnostic_pure.gc`
fail_compilation/attributediagnostic_pure.d(15):        which wasn't inferred `pure` because of:
fail_compilation/attributediagnostic_pure.d(15):        `asm` statement is assumed to be impure - mark it with `pure` if it is not
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
