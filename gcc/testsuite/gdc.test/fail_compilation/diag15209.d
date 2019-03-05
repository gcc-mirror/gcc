/*
TEST_OUTPUT:
---
fail_compilation/diag15209.d(18): Error: need 'this' for 'x' of type 'int'
fail_compilation/diag15209.d(21): Error: need 'this' for 'x' of type 'int'
---
*/

class C1 { int x; }
struct S1 { alias y = C1.x; }

struct S2 { int x; }
class C2 { alias y = S2.x; }

void main()
{
    S1 s1;
    s1.y = 10;  // invalid field variable access

    auto c2 = new C2();
    c2.y = 10;  // invalid field variable access
}
