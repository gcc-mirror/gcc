/*
TEST_OUTPUT:
---
fail_compilation/diag15186.d(14): Error: use `.` for member lookup, not `::`
fail_compilation/diag15186.d(15): Error: use `.` for member lookup, not `->`
---
*/

void main()
{
    struct S { static int x; int y; }
    S* s;

    S::x = 1;
    s->y = 2;
}
