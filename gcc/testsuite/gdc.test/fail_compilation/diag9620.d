/*
TEST_OUTPUT:
---
fail_compilation/diag9620.d(18): Error: pure function 'diag9620.main.bar' cannot call impure function 'diag9620.foo1'
fail_compilation/diag9620.d(19): Error: pure function 'diag9620.main.bar' cannot call impure function 'diag9620.foo2!().foo2'
---
*/

int x;

void foo1() { x = 3; }
void foo2()() { x = 3; }

void main() pure
{
    void bar()
    {
        foo1();
        foo2();
    }
}
