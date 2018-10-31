/*
TEST_OUTPUT:
---
fail_compilation/fail2361.d(13): Error: cannot modify immutable expression c
---
*/

class C {}

void main()
{
    immutable c = new immutable(C);
    delete c;
}
