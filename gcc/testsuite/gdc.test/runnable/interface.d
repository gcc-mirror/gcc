/*
TEST_OUTPUT:
---
runnable/interface.d(41): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
runnable/interface.d(55): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
---
*/

import core.stdc.stdio;

/*******************************************/

interface IStream
{
    int read();
}

interface OStream
{
    int write();
}

class IO : IStream, OStream
{
    int read() { return 7; }
    int write() { return 267; }
}

void foo(IStream i, OStream o)
{
    printf("foo(i = %p, o = %p)\n", i, o);
    assert(i.read() == 7);
    assert(o.write() == 267);
}

void test1()
{
    IO io = new IO();
    printf("io = %p\n", io);
    foo(io, io);
    delete io;
}

/*******************************************/

interface I { }
class C : I
{
    ~this() { printf("~C()\n"); }
}

void test2()
{
    I i = new C();
    delete i;

  {
    scope I j = new C();
  }
}

/*******************************************/

interface I3 {
    void h();
}
interface K3 {
    void f();
}

interface J3 : I3, K3 {}

class A3 : J3 {
    short x = 3;

    void f(){ assert(x == 3); }
    void h(){ assert(x == 3); }
}

void test3()
{
    auto a = new A3();
    a.f();
    a.h();
    J3 j = a;
    j.f();
    K3 k = a;
    k.f();
    assert(&j.f == &k.f); // https://issues.dlang.org/show_bug.cgi?id=3706
}

/*******************************************/

int main()
{
    test1();
    test2();
    test3();

    printf("Success\n");
    return 0;
}
