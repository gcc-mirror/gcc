
import core.stdc.stdio;

struct S { int a,b,c,d; }

alias int delegate() dg_t;
alias int delegate(int) dg1_t;

void fill()
{
    int[100] x;
}

/************************************/

dg_t foo()
{
    int x = 7;

    int bar()
    {
        return x + 3;
    }

    return &bar;
}

void test1()
{
    dg_t dg = foo();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo2()
{
    dg_t abc()
    {
        int x = 7;

        int bar()
        {
            return x + 3;
        }

        return &bar;
    }

    return abc();
}

void test2()
{
    dg_t dg = foo2();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo3()
{
    dg_t abc(int x)
    {
        int bar()
        {
            return x + 3;
        }

        return &bar;
    }

    return abc(7);
}

void test3()
{
    dg_t dg = foo3();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo4()
{
    S s;

    s = S(4,5,6,7);

    dg_t abc(S t)
    {
        int bar()
        {
            return t.d + 3;
        }

        return &bar;
    }

    return abc(s);
}

void test4()
{
    dg_t dg = foo4();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

void test5()
{
    int x = 7;

    dg_t abc(ref int y)
    {
        int bar()
        {
            y += 4;
            return y + 3;
        }

        return &bar;
    }

    dg_t dg = abc(x);
    fill();
    assert(x == 7);
    auto i = dg();
    assert(x == 11);
    assert(i == 14);

    x = 8;
    i = dg();
    assert(x == 12);
    assert(i == 15);
}

/************************************/

void test6()
{
    int x = 7;

    dg_t abc(out int y)
    {
        int bar()
        {
            y += 4;
            return y + 3;
        }

        return &bar;
    }

    dg_t dg = abc(x);
    fill();

    assert(x == 0);
    auto i = dg();
    assert(x == 4);
    assert(i == 7);

    x = 8;
    i = dg();
    assert(x == 12);
    assert(i == 15);
}

/************************************/

void test7()
{
    int[3] a = [10,11,12];

    dg_t abc(int[3] y)
    {
        int bar()
        {
            y[2] += 4;
            return y[2] + 3;
        }

        return &bar;
    }

    dg_t dg = abc(a);
    fill();

    assert(a[2] == 12);
    auto i = dg();
    assert(a[2] == 12);
    assert(i == 19);
}

/************************************/

void test8()
{
    S s = S(7,8,9,10);

    dg_t abc(ref S t)
    {
        int bar()
        {
            t.d += 4;
            return t.c + 3;
        }

        return &bar;
    }

    dg_t dg = abc(s);
    fill();

    assert(s.d == 10);
    auto i = dg();
    assert(s.d == 14);
    assert(i == 12);
}

/************************************/

S foo9(out dg_t dg)
{
    S s1 = S(7,8,9,10);

    dg_t abc()
    {
        int bar()
        {
            s1.d += 4;
            return s1.c + 3;
        }

        return &bar;
    }

    dg = abc();
    return s1;
}

void test9()
{
    dg_t dg;

    S s = foo9(dg);
    fill();
    assert(s.a == 7);
    assert(s.b == 8);
    assert(s.c == 9);
    assert(s.d == 10);

    auto i = dg();
    assert(s.d == 10);
    assert(i == 12);
}

/************************************/

dg_t foo10()
{
    dg_t abc()
    {
        int x = 7;

        int bar()
        {
            int def()
            {
                return x + 3;
            }
            return def();
        }

        return &bar;
    }

    return abc();
}

void test10()
{
    dg_t dg = foo10();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}


/************************************/

dg_t foo11()
{
    int x = 7;

    class T
    {
        int bar()
        {
            return x + 3;
        }
    }

    T t = new T;

    return &t.bar;
}

void test11()
{
    dg_t dg = foo11();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo12()
{
    int x = 7;

    class T
    {
        int bar()
        {
            return x + 3;
        }

        int xyz()
        {
            return bar();
        }
    }

    T t = new T;

    return &t.xyz;
}

void test12()
{
    dg_t dg = foo12();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo13()
{
    int x = 7;

    class T
    {
        int xyz()
        {
            int bar()
            {
                return x + 3;
            }

            return bar();
        }
    }

    T t = new T;

    return &t.xyz;
}

void test13()
{
    dg_t dg = foo13();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}


/************************************/

dg_t foo14()
{
    class T
    {
        int xyz()
        {
            int x = 7;

            int bar()
            {
                return x + 3;
            }

            return bar();
        }
    }

    T t = new T;

    return &t.xyz;
}

void test14()
{
    dg_t dg = foo14();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo15()
{
    class T
    {
        int x = 7;

        int xyz()
        {
            int bar()
            {
                return x + 3;
            }

            return bar();
        }
    }

    T t = new T;

    return &t.xyz;
}

void test15()
{
    dg_t dg = foo15();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 10);
}

/************************************/

dg_t foo16()
{
    int a = 5;

    class T
    {
        int x = 7;

        int xyz()
        {
            int y = 8;
            int bar()
            {
                return a + x + y + 3;
            }

            return bar();
        }
    }

    T t = new T;

    return &t.xyz;
}

void test16()
{
    dg_t dg = foo16();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 23);
}

/************************************/

dg_t foo17()
{
    int a = 5;

    class T
    {
        int x = 7;

        dg_t xyz()
        {
            int y = 8;

            int bar()
            {
                return a + x + y + 3;
            }

            return &bar;
        }
    }

    T t = new T;

    return t.xyz();
}

void test17()
{
    dg_t dg = foo17();
    fill();
    printf("bar = %d\n", dg());
    assert(dg() == 23);
}

/************************************/

dg_t dg18;

void bar18()
{
    int a = 7;
    int foo() { return a + 3; }

    dg18 = &foo;
    int i = dg18();
    assert(i == 10);
}

void test18()
{
    bar18();
    fill();
    int i = dg18();
    assert(i == 10);
}

/************************************/

void abc19(void delegate() dg)
{
    dg();
    dg();
    dg();
}

struct S19
{
    static S19 call(int v)
    {
        S19 result;

        result.v = v;
        void nest()
        {
            result.v += 1;
        }
        abc19(&nest);
        return result;
    }
    int a;
    int v;
    int x,y,z;
}

int foo19()
{
    auto s = S19.call(5);
    return s.v;
}

void test19()
{
    int i = foo19();
    printf("%d\n", i);
    assert(i == 8);
}

/************************************/

void enforce20(lazy int msg)
{
}


void test20()
{
    int x;
    foreach (j; 0 .. 10)
    {
        printf("%d\n", j);
        assert(j == x);
        x++;
        enforce20(j);
    }
}

/************************************/

void thrash21() { char[128] x = '\xfe'; }

void delegate() dg21;
int g_input = 11, g_output;

void f21()
{
    int i = g_input + 2;

    class X
    {
        // both 'private' and 'final' to make non-virtual
        private final void actual()
        {
            g_output = i;
        }

        void go()
        {
            actual();
        }
    }

    dg21 = & (new X).go;
}

void test21()
{
    f21();
    thrash21();
    dg21();
    assert(g_output == 13);
}

/************************************/

void thrash22() { char[128] x = '\xfe'; }
int gi22;
void delegate() dg22;

class A22
{
    int x = 42;

    void am()
    {
        int j; /* Making f access this variable causes f's chain to be am's
                  frame.  Otherwise, f's chain would be the A instance. */
        void f()
        {
            int k = j;

            void g()
            {
                class B
                {
                    void bm()
                    {
                        gi22 = x; /* No checkedNestedReference for A.am.this,
                                   so it is never placed in a closure. */
                    }
                }

                (new B).bm();
            }

            dg22 = &g;
        }

        f();
    }
}

void test22()
{
    (new A22).am();
    thrash22();
    dg22();
    assert(gi22 == 42);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=1759

void test1759()
{
    struct S { int a, b, c; }
    struct SS { S obj; }

    static int delegate() makeSum1(S s)
    {
        with (s) return { return a + b + c; };
    }
    static int delegate() makeSum2(S[1] sa)
    {
        with (sa[0]) return { return a + b + c; };
    }
    static int delegate() makeSum3(SS ss)
    {
        with (ss.obj) return { return a + b + c; };
    }
    static int delegate() makeSum4(SS[1] ssa)
    {
        with (ssa[0].obj) return { return a + b + c; };
    }

    S s = {15, 30, 45};
    SS ss = {s};
    int delegate() sum;

    sum = makeSum1(s);      assert(sum() == 90);
    sum = makeSum2([s]);    assert(sum() == 90);
    sum = makeSum3(ss);     assert(sum() == 90);
    sum = makeSum4([ss]);   assert(sum() == 90);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=1841

int delegate() foo1841()
{
    int stack;
    int heap = 3;

    int nested_func()
    {
        ++heap;
        return heap;
    }
    return delegate int() { return nested_func(); };
}

int delegate() foo1841b()
{
    int stack;
    int heap = 7;

    int nested_func()
    {
        ++heap;
        return heap;
    }
    int more_nested() { return nested_func(); }
    return delegate int() { return more_nested(); };
}

void test1841()
{
    auto z = foo1841();
    auto p = foo1841();
    assert(z() == 4);
    p();
    assert(z() == 5);
    z = foo1841b();
    p = foo1841b();
    assert(z() == 8);
    p();
    assert(z() == 9);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=5911

void writeln5911(const(char)[] str) {}

void logout5911(lazy const(char)[] msg) { writeln5911(msg); }

void test5911()
{
    string str = "hello world";
    logout5911((){ return str; }());    // closure 1

    try
    {
        throw new Exception("exception!!");
    }
    catch (Exception e)
    {
        assert(e !is null);
        logout5911(e.toString());       // closure2 SEGV : e is null.
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=9685

auto get9685a(alias fun)()
{
    int x = 10;
    struct Foo
    {
        size_t data;

        @property clone()
        {
            return Foo(15);
        }
    }
    return Foo(5);
}
void test9685a()
{
    uint a = 42;
    auto bar = get9685a!(() => a)();
    auto qux = bar.clone;
    //printf("bar context pointer : %p\n", bar.tupleof[$-1]);
    //printf("qux context pointer : %p\n", qux.tupleof[$-1]);
    assert(bar.tupleof[$-1] == qux.tupleof[$-1]);
    assert(qux.data == 15);
}

auto get9685b(alias fun)()
{
    int x = 10;
    struct Foo
    {
        size_t data;

        @property clone()
        {
            return Foo(data + x);
        }
    }
    return Foo(5);
}
void test9685b()
{
    uint a = 42;
    auto bar = get9685b!(() => a)();
    auto qux = bar.clone;
    //printf("bar context pointer : %p\n", bar.tupleof[$-1]);
    //printf("qux context pointer : %p\n", qux.tupleof[$-1]);
    assert(bar.tupleof[$-1] == qux.tupleof[$-1]);
    assert(qux.data == 15);
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=12406

auto createDg12406()
{
    static struct Dg
    {
        Dg delegate() action;
    }

    static void fn(void delegate()) { }

    int x; fn({ x++; }); // required

    Dg dg;

    Dg createDg2()
    {
        int x; void unusedFun() { x++; } // required

        return Dg(() => dg); // lambda returns garbage instead of dg
    }

    return dg = Dg(&createDg2);
}

void test12406()
{
    auto dgs = [createDg12406()];
    //printf("dgs[%2d].action = %p:%p\n", 0, dgs[$-1].action.ptr, dgs[$-1].action.funcptr);
    foreach (i; 1 .. 10+1)
    {
        dgs ~= dgs[i-1].action();
        //printf("dgs[%2d].action = %p:%p\n", i, dgs[$-1].action.ptr, dgs[$-1].action.funcptr);
    }

    foreach (i, dgx; dgs)
    {
        if (i % 2 == 0)
        {
            // All closures are equal with dgs[0].
            assert(dgx.action.ptr     is dgs[0].action.ptr);
            assert(dgx.action.funcptr is dgs[0].action.funcptr);    // is: createDg2
        }
        else
        {
            // Each closures has unique context.
            for (size_t j = i + 2; j < dgs.length; j += 2)
                assert(dgx.action.ptr !is dgs[j].action.ptr);
            assert(dgx.action.funcptr is dgs[1].action.funcptr);    // is: lambda () => dg
        }
    }
}

/************************************/
// https://issues.dlang.org/show_bug.cgi?id=14730

void test14730()
{
    static auto makeS(int x)
    {
        struct S
        {
            int n;
            int get() { return x; }     // x will be a closure variable
        }
        return S(x);
    }
    auto s = makeS(1);
    assert(s.get() == 1);
    // By inlining get() function call, it's rewritten to:
    // assert(*(s.tupleof[$-1] + x.offset) == 1);
    // --> In DotVarExp::toElem(), x->offset should be already nonzero.
}

// ----

// This is questionable case. Currently it works without any errors,
// but not sure it's really intentional
// It showed up again in https://issues.dlang.org/show_bug.cgi?id=23112
// where it's an @safe issue so it's a bug.
static if (0)
{
struct S14730x(alias f)
{
    auto foo()() { return f(0); }

    void dummy() {}
}

auto makeS14730x() //@nogc
{
    int x = 10;
    S14730x!(a => x) s;
    //assert(s.foo() == 10);
    return s;
}

void test14730x()
{
    auto s = makeS14730x();
    assert(s.tupleof[$-1] !is null);

    // instantiationg foo outside of makeS will place the variable x in closure
    // *after* the semantic3 completion of makeS() function.
    assert(s.foo() == 10);
}
}

/************************************/

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
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test22();
    test1759();
    test1841();
    test5911();
    test9685a();
    test9685b();
    test12406();
    test14730();
    //test14730x();

    printf("Success\n");
    return 0;
}
