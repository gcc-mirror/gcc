// RUNNABLE_PHOBOS_TEST
import std.stdio;

/*******************************************************/

class Outer
{
    int o;
    static int os = 3;

    static class StaticInner
    {
        int si;

        int foo()
        {
            return si + os;
        }
    }

    class Inner
    {
        int i;

        int foo(int m)
        {
            return i * o + m;
        }

        this(int k)
        {
            i = k + 7;
            os = 6;
        }
    }

    Inner bar()
    {
        return new Inner(17);
    }
}

void test1()
{
    Outer p = new Outer();

    assert(p.o == 0);
    assert(p.os == 3);
    p.o = 9;

    Outer.StaticInner s = new Outer.StaticInner;
    s.si = 10;
    assert(s.foo() == 13);

    Outer.Inner q = p.bar();
    assert(q.i == 24);
    assert(q.foo(8) == 24*9+8);
}

/*******************************************************/

class C1
{
    int c1;

    this()
    {
        c1 = 2;
    }

    class C2
    {
        class C3
        {
            int c3;

            this(int n)
            {
                c3 = n + c1 + c2;
            }
        }

        int c2;

        C3 foo()
        {
            return new C3(8);
        }

        this(int k)
        {
            c2 = k + 7;
        }
    }

    C2 bar()
    {
        return new C2(17);
    }
}

void test2()
{
   C1 p = new C1();
   assert(p.c1 == 2);

   C1.C2 q = p.bar();
   assert(q.c2 == 24);

   C1.C2.C3 r = q.foo();
   assert(r.c3 == 2+24+8);
}

/*******************************************************/

class C3
{
    int c1;

    this()
    {
        c1 = 2;
    }

    class B { }

    class C2 : B
    {
        int c2;

        this(int k)
        {
            c2 = c1 + k + 7;
        }
    }

    C2 bar()
    {
        return new C2(17);
    }
}

void test3()
{
   C3 q = new C3();
   assert(q.c1 == 2);
   C3.C2 p = q.bar();
   assert(p.c2 == 26);
}


/*******************************************************/

void test4()
{
    int m = 3;

    class C
    {
        void foo()
        {
            assert(m == 3);
        }
    }

    void bar()
    {
        assert(m == 3);
    }

    bar();
    C c = new C;
    c.foo();
}

/*******************************************************/

void test5()
{
    int m = 3;

    void bar()
    {
        assert(m == 3);
    }

    class C
    {
        void foo()
        {
            assert(m == 3);
            bar();
        }
    }

    bar();
    C c = new C;
    c.foo();
}


/*******************************************************/

void test6()
{
    int m = 3;

    void bar()
    {
        assert(m == 3);
    }

    void abc()
    {
        class C
        {
            void foo()
            {
                assert(m == 3);
                bar();
            }
        }

        bar();
        C c = new C;
        c.foo();
    }

    abc();
}

/*******************************************************/

void test7()
{
    int m = 3;

    void bar()
    {
        assert(m == 3);
    }

    void ghi()
    {
        void abc()
        {
            class C
            {
                void foo()
                {
                    assert(m == 3);
                    bar();
                }
            }

            bar();
            C c = new C;
            c.foo();
        }

        abc();
    }

    ghi();
}

/*******************************************************/

void test8()
{
    int m = 3;

    void bar()
    {
        assert(m == 3);
    }

    void ghi()
    {
        void abc()
        {
            class C
            {
                void foo()
                {
                    void fooey()
                    {
                        assert(m == 3);
                        bar();
                    }

                    fooey();
                }
            }

            bar();
            C c = new C;
            c.foo();
        }

        abc();
    }

    ghi();
}

/*******************************************************/

class C9
{
    int m;

    void ccc()
    {
        assert(m == 3);
    }

    this()
    {
        m = 3;
    }

    class D
    {
        int n;

        this() { n = 4; }

        void abc()
        {
            assert(m == 3);
            ccc();
        }
    }

    D def()
    {
        return new D;
    }
}


void test9()
{
    C9 c = new C9;
    assert(c.m == 3);
    c.ccc();

    C9.D d = c.def();
    assert(d.n == 4);
    d.abc();
}

/*******************************************************/

void test10()
{
    int m = 3;

    class C
    {
        void foo()
        {
            assert(m == 3);
        }
    }

    void abc()
    {
        C c = new C;
        c.foo();
    }

    abc();
}

/*******************************************************/

void test11()
{
    int m = 3;

    class C
    {
        void foo()
        {
            assert(m == 3);
        }
    }

    void abc()
    {
        void def()
        {
            C c = new C;
            c.foo();
        }

        def();
    }

    abc();
}

/*******************************************************/

void test12()
{
    int m = 3;

    class C
    {
        void foo()
        {
            assert(m == 3);
        }

        void abc()
        {
            assert(m == 3);
            C c = new C;
            c.foo();
        }
    }

    C d = new C;
    d.abc();
}

/*******************************************************/

void test13()
{
    int m = 3;

    class C
    {
        void foo()
        {
            assert(m == 3);
        }

        void abc()
        {
            void def()
            {
                assert(m == 3);
                C c = new C;
                c.foo();
            }

            def();
        }
    }

    C d = new C;
    d.abc();
}

/*******************************************************/

class C14
{
    void foo()
    {
        assert(0);
    }
}

void test14()
{
    int m = 3;

    C14 c = new class C14
        {
            override void foo()
            {
                printf("in inner class\n");
                assert(m == 3);
            }
        };

    c.foo();
}

/*******************************************************/

class C15
{
    void foo()
    {
        assert(0);
    }
}

void test15()
{
    int m = 3;

    C15 c = new class(1,2) C15
        {
            this(int i, int j)
            {
                printf("in inner class ctor\n");
                assert(i == 1);
                assert(j == 2);
            }

            override void foo()
            {
                printf("in inner class\n");
                assert(m == 3);
            }
        };

    c.foo();
}

/*******************************************************/

class C16
{
    int w = 3;

    void iterator()
    {
        class Foo
        {
            void current()
            {
                assert(w == 3);
            }
        }

        Foo f = new Foo();
        f.current();
    }
}

void test16()
{
    C16 s = new C16();

    s.iterator();
}

/*******************************************************/

class Owner
{

    this()
    {
        n = new Nested(this);
    }

    class Nested
    {
        this(Owner owner)
        {
            m = owner;
        }

        Owner foo()
        {
            return m;
        }

        Owner m;
    }

    Nested n;
}

class OwnerDerived : Owner
{
}

void test17()
{
    OwnerDerived o = new OwnerDerived();

    assert(o.n.foo() is o);
}

/*******************************************************/

class Foo18
{
    class Bar
    {
        void doSayHello()
        {
            writefln("Betty");
            sayHello();
        }
    }
    Bar bar;

    void sayHello()
    {
        writefln("Hello");
    }
}

class Foo182 : Foo18
{
    this()
    {
        bar = new Bar();
    }
}

void test18()
{
    Foo182 foo = new Foo182();
    writefln("This should print Hello:");

    foo.bar.doSayHello();
}

/*******************************************************/

class Foo19
{
    class Bar
    {
        void doSayHello()
        {
            writefln("Betty");
            sayHello();
        }
    }
    Bar bar;

    void sayHello()
    {
        writefln("Hello");
    }

    this()
    {
        bar = new Bar();
    }
}

class Foo192 : Foo19
{
}

void test19()
{
    Foo192 foo = new Foo192();
    writefln("This should print Hello:");

    foo.bar.doSayHello();
}

/*******************************************************/

class Outer20
{
    int a;

    class Inner
    {
        int foo()
        {
            return a;
        }
    }
}

void test20()
{
    Outer20 o = new Outer20;
    o.a = 3;
    Outer20.Inner oi = o.new Inner;
    assert(oi.foo() == 3);
}

/*******************************************************/

class Foo21{}

static if (is(typeof(new class Foo21{}))) {}

void test21()
{
}

/*******************************************************/

class Outer22
{
    class Inner
    {
        Outer22 foo()
        {
            return this.outer;
        }
    }

    void bar()
    {
        Inner i = new Inner;
        assert(this == i.foo());
    }
}

void test22()
{
    Outer22 o = new Outer22;
    o.bar();
}

/*******************************************************/

class Adapter23
{
    void func() { }
}

class Foo23
{
    class AnonAdapter : Adapter23
    {
    }

    void bar()
    {
        Adapter23 a = cast( Adapter23 )( new AnonAdapter() );
    }
}

void test23()
{
    Foo23 f = new Foo23();
    f.bar();
    Adapter23 a = cast( Adapter23 )( f.new AnonAdapter() );
    auto aa = f.new AnonAdapter();
    Adapter23 ab = cast( Adapter23 )(aa);

}

/*******************************************************/

class I24
{
    public abstract void callI();
}

C24 c24;

class C24
{
    private int index;
    void foo()
    {
        printf( "ok, this = %p\n", this);
        assert(this == c24);
    }
    I24 bar()
    {
        auto i = new class() I24
        {
            override public void callI()
            {
                foo();
            }
        };
        printf("bar.this = %p\n", this);
        printf("  i.this = %p\n", (cast(void**)i)[2]);
        assert(*cast(void**)&c24 == (cast(void**)i)[2]);
        return i;
    }
}

void test24()
{
    c24 = new C24;
    printf("c = %p\n", c24);
    auto i = c24.bar();
    i.callI();
}

/*******************************************************/

struct S7426
{
    static struct Inner
    {
        int x;
        alias typeof(Inner.tupleof) T;
    }
}

/*******************************************************/
// 14046

class A14046
{
    class NestedA { }
}

class B14046 : A14046
{
    int field;

    class NestedB : NestedA
    {
        void foo()
        {
            this.outer.field = 1;  // ok <- disallowed

            //(cast(B14046)this.outer).field = 1;  // workaround
        }
    }
}

void test14046()
{
    auto b = new B14046();
    auto nb = b.new NestedB();
    assert(b.field == 0);
    nb.foo();
    assert(b.field == 1);
}

/*******************************************************/
// 15839

class AnimatedProgress15839(bool makeClosure)
{
    static AnimatedProgress15839 saveThis;

    interface Runnable {}

    static class GC
    {
        this(AnimatedProgress15839 ap)
        {
            assert(ap is saveThis);
        }
    }

    void start()
    {
        assert(this is saveThis);

        static if (makeClosure) int a;

        auto r = new class Runnable
        {
            void run()
            {
                static assert(is(typeof(this.outer) == AnimatedProgress15839));
                assert(this.outer is saveThis);

                GC gc = new GC(this.outer);

                static if (makeClosure) int b = a;
            }
        };
        r.run();
    }
}

void test15839()
{
    auto ap1 = new AnimatedProgress15839!false();
    ap1.saveThis = ap1;
    ap1.start();

    auto ap2 = new AnimatedProgress15839!true();
    ap2.saveThis = ap2;
    ap2.start();
}

/*******************************************************/

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
    test23();
    test24();

    test14046();
    test15839();

    printf("Success\n");
    return 0;
}
