/*
TEST_OUTPUT:
---
instantiating...
instantiating...
last instantiation!!!
---

RUN_OUTPUT:
---
1
3
Success
---
*/

import core.stdc.stdio;

/*********************************************************/

template Foo(T)
{
    static if (is(T : int))
        alias T t1;

    static if (T.sizeof == 4)
        alias T t2;

    static if (is(T U : int))
        alias U t3;

    static if (is(T* V : V*))
        alias V t4;

    static if (is(T W))
        alias W t5;
    else
        alias char t5;

    static if (is(T* X : X*))
    {
    }
}

void test1()
{
    Foo!(int).t1 x1;
    assert(typeid(typeof(x1)) == typeid(int));

    Foo!(int).t2 x2;
    assert(typeid(typeof(x2)) == typeid(int));

    Foo!(int).t3 x3;
    assert(typeid(typeof(x3)) == typeid(int));

    Foo!(int*).t4 x4;
    assert(typeid(typeof(x4)) == typeid(int*));

    Foo!(int).t5 x5;
    assert(typeid(typeof(x5)) == typeid(int));

    Foo!(int).X x6;
    assert(typeid(typeof(x6)) == typeid(int));
}

/*********************************************************/


void test2()
{
    alias int T;

    static if (is(T : int))
        alias T t1;

    static if (T.sizeof == 4)
        alias T t2;

    static if (is(T U : int))
        alias U t3;

    static if (is(T* V : V*))
        alias V t4;

    static if (is(T W))
        alias W t5;
    else
        alias char t5;

    static if (is(T* X : X*))
    {
    }

    t1 x1;
    assert(typeid(typeof(x1)) == typeid(int));

    t2 x2;
    assert(typeid(typeof(x2)) == typeid(int));

    t3 x3;
    assert(typeid(typeof(x3)) == typeid(int));

    t4 x4;
    assert(typeid(typeof(x4)) == typeid(int));

    t5 x5;
    assert(typeid(typeof(x5)) == typeid(int));

    X x6;
    assert(typeid(typeof(x6)) == typeid(int));
}

/*********************************************************/

void test3()
{
    static if (is(short : int))
    {   printf("1\n");
    }
    else
        assert(0);
    static if (is(short == int))
        assert(0);
    static if (is(int == int))
    {   printf("3\n");
    }
    else
        assert(0);
}

/*********************************************************/

template TValue(int i:1)
{
        pragma(msg,"last instantiation!!!");
        const int TValue = 1;
}

template TValue(int i)
{
        pragma(msg,"instantiating...");
        const int TValue = i * TValue!(i-1);
}

void test4()
{
        assert(TValue!(3) == 6);
}

/*********************************************************/

template Reverse(string s: "") {
    const char[] Reverse = "";
}

template Reverse(string s) {
    const char[] Reverse = Reverse!(s[1..$]) ~ s[0];
}

void test5()
{
    assert(Reverse!("Recursive string template") == "etalpmet gnirts evisruceR");
}

/*********************************************************/

template foo6(alias V)
{
    int foo6()
    {
        return V;
    }
}

class bar6(alias V)
{
    int abc()
    {
        return V;
    }
}

void test6()
{
    int j = 3;
    int k = 4;

    int i = foo6!(j)();
    i += foo6!(j)();

    i += foo6!(k)();

    bar6!(j) b = new bar6!(j);
    i -= b.abc();

    assert(i == 7);
}

/*********************************************************/

template Bind7(alias dg)
{
    int Bind7()
    {
        dg('c');
        return 0;
    }
}

void test7()
{
    char[] v;

    void foo(char c) { v ~= c; }

    alias Bind7!(foo) intv;
    intv();
    assert(v[0] == 'c');
}

/*********************************************************/

template sum8(real x)
{
    static if (x <= 1.0L){
            const real sum8 = x;
    }else{
            const real sum8 = x + sum8!(x - 1.0L);
    }
}

void test8()
{
    real x = sum8!(3.0L);

    if(x != 6.0L){
            assert(0);
    }
}

/*********************************************************/

// https://github.com/dlang/dmd/issues/20907
struct Bar { enum bar = 1; }
void foo(A)(A[], A[1 << A.bar]) {}
void test20907()
{
    foo((Bar[]).init, (Bar[2]).init);
}

/*********************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test20907();

    printf("Success\n");
    return 0;
}
