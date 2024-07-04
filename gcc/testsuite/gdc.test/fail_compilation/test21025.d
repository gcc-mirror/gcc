// https://issues.dlang.org/show_bug.cgi?id=21025
// REQUIRED_ARGS: -preview=dip1021

/*
TEST_OUTPUT:
---
fail_compilation/test21025.d(15): Error: variable `r` cannot be read at compile time
fail_compilation/test21025.d(15):        called from here: `binaryFun(r, r)`
fail_compilation/test21025.d(24): Error: template `uniq` is not callable using argument types `!()(void[])`
fail_compilation/test21025.d(14):        Candidate is: `uniq()(int[] r)`
---
*/

void uniq()(int[] r)
if (binaryFun(r, r)) {}

bool binaryFun(T, U)(T, U)
{
    return true;
}

void generateStatements()
{
    uniq([]);
}
