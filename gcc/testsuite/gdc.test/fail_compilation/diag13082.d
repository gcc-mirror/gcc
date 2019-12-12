/*
TEST_OUTPUT:
---
fail_compilation/diag13082.d(22): Error: constructor diag13082.C.this (int a) is not callable using argument types (string)
fail_compilation/diag13082.d(23): Error: constructor diag13082.S.this (int a) is not callable using argument types (string)
---
*/

class C
{
    this(int a) {}
}

struct S
{
    this(int a) {}
}

void main()
{
    string b;
    auto c = new C(b);
    auto s = new S(b);
}
