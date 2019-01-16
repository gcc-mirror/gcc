// { dg-lto-do link }
module ltotests_0;

import core.stdc.stdio;


/******************************************/

interface I284
{
   void m284();
}

class C284 : I284
{
   void m284() { }
}

/******************************************/

class C304
{
}

C304 c304;

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=61

struct S61a
{
    void a() { }
    void b() { }
}

struct S61b
{
    S61a other;

    void foo()
    {
        bar();
    }

    void bar()
    {
        try
            other.a();
        catch
            other.b();
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=88

extern(C) int test88a();

void test88()
{
    test88a();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=252

class C252
{
    struct S252
    {
        int i;
        ubyte u;
    }
    S252 s;
}

void test252()
{
    C252 c = new C252();
}

/******************************************/

void main(string[])
{
    test88();
    test252();

    printf("Success!\n");
}
