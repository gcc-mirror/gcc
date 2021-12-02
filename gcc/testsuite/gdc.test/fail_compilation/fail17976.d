/*
TEST_OUTPUT:
---
fail_compilation/fail17976.d(11): Error: constructor `fail17976.S.this` parameter `this.a` is already defined
fail_compilation/fail17976.d(11): Error: constructor `fail17976.S.this` parameter `this.a` is already defined
---
*/

struct S
{
    this(string a, string a, string a)
    {
    }
}

void main()
{
}
