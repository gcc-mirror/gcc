/*
TEST_OUTPUT:
---
fail_compilation/fail106.d(12): Error: cannot modify `immutable` expression `"ABC"[2]`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=239
// Internal error: changing string literal elements
void main()
{
    "ABC"[2] = 's';
}
