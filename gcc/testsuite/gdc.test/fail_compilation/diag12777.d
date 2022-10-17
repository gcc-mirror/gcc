/*
TEST_OUTPUT:
---
fail_compilation/diag12777.d(14): Error: cannot modify `this.v` in `const` function
fail_compilation/diag12777.d(15): Error: cannot modify `this.v` in `immutable` function
fail_compilation/diag12777.d(21): Error: cannot modify `this.v` in `const` function
fail_compilation/diag12777.d(22): Error: cannot modify `this.v` in `immutable` function
---
*/

struct S
{
    int v;
    void fun() const     { v++; }
    void gun() immutable { v++; }
}

class C
{
    int v;
    void fun() const     { v++; }
    void gun() immutable { v++; }
}
