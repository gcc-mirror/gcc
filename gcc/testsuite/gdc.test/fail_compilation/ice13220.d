/*
TEST_OUTPUT:
---
fail_compilation/ice13220.d(22): Error: template instance test!0 does not match template declaration test(T)()
---
*/

struct Tuple(T...)
{
    T field;
    alias field this;
}

template test(T)
{
    bool test() { return false; };
}

void main()
{
    Tuple!bool t;
    t[0] = test!0();
}
