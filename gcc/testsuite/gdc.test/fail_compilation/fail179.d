/*
TEST_OUTPUT:
---
fail_compilation/fail179.d(11): Error: variable fail179.main.px cannot be final, perhaps you meant const?
---
*/

void main()
{
    int x = 3;
    final px = &x;
    *px = 4;
    auto ppx = &px;
    **ppx = 5;
}
