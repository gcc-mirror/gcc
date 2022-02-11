/*
TEST_OUTPUT:
---
fail_compilation/diag10984.d(12): Error: `static` function `diag10984.f.n` cannot access variable `x` in frame of function `diag10984.f`
fail_compilation/diag10984.d(11):        `x` declared here
---
*/

void f()
{
    int x;
    static void n() { x++; }
}

void main()
{
}
