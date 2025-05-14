/*
TEST_OUTPUT:
---
fail_compilation/misc1.d(109): Error: `5` has no effect
fail_compilation/misc1.d(110): Error: `1 + 2` has no effect
fail_compilation/misc1.d(111): Error: `x` has no effect
fail_compilation/misc1.d(117): Deprecation: `1 * 1` has no effect
fail_compilation/misc1.d(118): Deprecation: `() { j++; d++; }` has no effect
fail_compilation/misc1.d(124): Deprecation: `false` has no effect
fail_compilation/misc1.d(127): Deprecation: `*sp++` has no effect
fail_compilation/misc1.d(128): Deprecation: `j` has no effect
---
*/

#line 100

/***************************************************/
//https://issues.dlang.org/show_bug.cgi?id=12490

void hasSideEffect12490(){}

void issue12490()
{
    int x;
    5, hasSideEffect12490();
    1 + 2, hasSideEffect12490();
    x, x++;
}

void issue23480()
{
    int j;
    for({} j; 1*1) {}
    for({j=2; int d = 3;} j+d<7; {j++; d++;}) {}
    for (
        if (true)        // (o_O)
            assert(78);
        else
            assert(79);
        false; false
    ) {}
    // unnecessary deref
    for (ubyte* sp; 0; *sp++) {}
    for (;; j) {}
}
