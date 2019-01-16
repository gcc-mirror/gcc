/*
TEST_OUTPUT:
---
fail_compilation/diag10984.d(11): Error: static function diag10984.f.n cannot access frame of function diag10984.f
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
