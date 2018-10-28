/*
TEST_OUTPUT:
---
fail_compilation/fail106.d(12): Error: cannot modify immutable expression 'C'
---
*/

// Issue 239 - Internal error: changing string literal elements

void main()
{
    "ABC"[2] = 's';
}
