/*
TEST_OUTPUT:
---
fail_compilation/fix350a.d(12): Error: comma expected separating field initializers
fail_compilation/fix350a.d(12): Error: comma expected separating field initializers
---
*/
struct S1
{
    int a, b, c;

    static immutable S1 C1 = { 1 2 3 }; // no commas here, compiles
    static immutable S1 C2 = { 1, 2, 3 }; // compiles as well
}
