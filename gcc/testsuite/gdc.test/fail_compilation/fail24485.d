// https://issues.dlang.org/show_bug.cgi?id=24485
/*
TEST_OUTPUT:
---
fail_compilation/fail24485.d(25): Error: cannot implicitly convert expression `*a` of type `A` to `B`
fail_compilation/fail24485.d(31): Error: cannot implicitly convert expression `this.a` of type `A` to `B`

---
*/

struct A
{
    int i = 43;
    this(ref A rhs) {}
}

struct B
{
    int i = 42;
}

ref B foo()
{
    auto a = new A;
    return *a;
}

struct C
{
    A a;
    @property ref B b() { return a; }
}
