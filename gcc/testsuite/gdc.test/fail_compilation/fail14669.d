/*
TEST_OUTPUT:
---
fail_compilation/fail14669.d(11): Error: 'auto' can only be used as part of 'auto ref' for template function parameters
fail_compilation/fail14669.d(16): Error: template instance fail14669.foo1!() error instantiating
fail_compilation/fail14669.d(12): Error: 'auto' can only be used as part of 'auto ref' for template function parameters
fail_compilation/fail14669.d(17): Error: template fail14669.foo2 cannot deduce function from argument types !()(int), candidates are:
fail_compilation/fail14669.d(12):        fail14669.foo2()(auto int a)
---
*/
void foo1()(auto int a) {}
void foo2()(auto int a) {}

void test1()
{
    alias f1 = foo1!();
    foo2(1);
}

/*
TEST_OUTPUT:
---
fail_compilation/fail14669.d(29): Error: 'auto' can only be used as part of 'auto ref' for template function parameters
fail_compilation/fail14669.d(38): Error: template instance fail14669.bar1!int error instantiating
fail_compilation/fail14669.d(30): Error: 'auto' can only be used as part of 'auto ref' for template function parameters
fail_compilation/fail14669.d(40): Error: template instance fail14669.bar2!int error instantiating
---
*/
void bar1(T)(auto ref T x) {}
void bar2(T)(auto ref T x) {}

void test2()
{
    int n;

    bar1(1);
    bar1(n);
    alias b1 = bar1!(int);

    alias b2 = bar2!(int);
    bar2(n);
    bar2(1);
}
