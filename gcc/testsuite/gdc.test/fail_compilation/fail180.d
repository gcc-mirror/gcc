/*
TEST_OUTPUT:
---
fail_compilation/fail180.d(23): Error: cannot modify this.x in const function
fail_compilation/fail180.d(24): Error: cannot modify this.x in const function
fail_compilation/fail180.d(38): Error: cannot modify this.x in const function
fail_compilation/fail180.d(39): Error: cannot modify this.x in const function
fail_compilation/fail180.d(50): Error: variable fail180.main.t cannot be final, perhaps you meant const?
fail_compilation/fail180.d(62): Error: variable fail180.test.d cannot be final, perhaps you meant const?
---
*/

struct S59
{
    int x;

    void foo()
    {
        x = 3;
    }
    const void bar()
    {
        x = 4;
        this.x = 5;
    }
}

class C
{
    int x;

    void foo()
    {
        x = 3;
    }
    const void bar()
    {
        x = 4;
        this.x = 5;
    }
}

void main()
{
    S59 s;

    s.foo();
    s.bar();

    final S59 t;
    t.foo();
    t.bar();
}

void test()
{
    C c = new C;

    c.foo();
    c.bar();

    final C d = new C;
    d.foo();
    d.bar();
}
