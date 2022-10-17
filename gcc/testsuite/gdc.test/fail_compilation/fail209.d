/*
TEST_OUTPUT:
---
fail_compilation/fail209.d(20): Error: incompatible types for `(a) -= (x)`: `float` and `fail209.X`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=725
// expression.c:6516: virtual Expression* MinAssignExp::semantic(Scope*): Assertion `e2->type->isfloating()' failed
class X
{
    float a;
}

void main()
{
    X x;
    float a;

    a -= x;
}
