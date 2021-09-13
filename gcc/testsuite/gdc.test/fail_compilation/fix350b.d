/*
TEST_OUTPUT:
---
fail_compilation/fix350b.d(15): Error: comma expected separating field initializers
fail_compilation/fix350b.d(15): Error: comma expected separating field initializers
fail_compilation/fix350b.d(16): Error: comma expected separating field initializers
---
*/
int foo() { return 3; }

struct S2
{
    int a, b, c;

    static immutable S2 C1 = { foo() 2 3 }; // compiles (and works)
    static immutable S2 C2 = { foo() 2, 3 }; // compiles (and works)
    //static immutable S2 C3 = { 2 foo() 3 }; // does not compile: comma expected separating field initializers
}
