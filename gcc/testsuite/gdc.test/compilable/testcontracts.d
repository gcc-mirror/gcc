// EXTRA_SOURCES: imports/testcontracts.d

import imports.testcontracts;

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=3602

class Derived3602 : Base3602
{
   override void method(int x, int y)
   in
   {
       assert(x > 0);
       assert(y > 0);
   }
   body
   {
   }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=5230

class Derived5230 : Base5230
{
    override int method()
    {
        return 69;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=17502
class Foo17502
{
    auto foo()
    out {}
    body {}

    auto bar()
    out { assert (__result > 5); }
    body { return 6; }

    auto bar_2()
    out (res) { assert (res > 5); }
    body { return 6; }

    int concrete()
    out { assert(__result > 5); }
    body { return 6; }

    int concrete_2()
    out(res) { assert (res > 5); }
    body { return 6; }

    void void_foo()
    out {}
    body {}

    auto void_auto()
    out {}
    body {}
}

/***************************************************/
// Order of declaration: (A), (C : B), (B : A)

class A17502
{
    int method(int p)
    in
    {
        assert(p > 5);
    }
    out(res)
    {
        assert(res > 5);
    }
    body
    {
        return p;
    }
}

class C17502 : B17502
{
    override int method(int p)
    in
    {
        assert(p > 3);
    }
    body
    {
        return p * 2;
    }
}

class B17502 : A17502
{
    override int method(int p)
    in
    {
        assert(p > 2);
    }
    body
    {
        return p * 3;
    }
}

/***************************************************/
// Order of declaration: (X : Y), (Y : Z), (Z)

class X17502 : Y17502
{
    override int method(int p)
    in
    {
        assert(p > 3);
    }
    body
    {
        return p * 2;
    }
}

class Y17502 : Z17502
{
    override int method(int p)
    in
    {
        assert(p > 2);
    }
    body
    {
        return p * 3;
    }
}

class Z17502
{
    int method(int p)
    in
    {
        assert(p > 5);
    }
    out(res)
    {
        assert(res > 5);
    }
    body
    {
        return p;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=17893

final class Foo17893(T)
{
    extern(C) void maythrow();

    void bar()
    in
    {
        maythrow();
    }
    body
    {
    }
}

Foo17893!int foo17893;
