// https://issues.dlang.org/show_bug.cgi?id=22292

// Original case

class C1
{
    C1 c1;
    this () pure
    {
        c1 = this;
    }
}
immutable x = cast(immutable)r;

auto r()
{
    C1 c1 = new C1;
    return c1;
}

// Reference stored in another class

template Test2()
{
    class C1
    {
        C2 c2;
        this () pure
        {
            C1 a = this;
            c2 = new C2(a);
        }
    }
    class C2
    {
        C1 c1;
        this (C1 c) pure
        {
            c1 = c;
        }
    }
    immutable x = cast(immutable)r;

    auto r()
    {
        C1 c1 = new C1();
        return c1;
    }
}

alias test2 = Test2!();

// Ditto but using a struct in the middle

template Test3()
{
    class C0
    {
        S1 s1;

        this()
        {
            s1 = S1(this);
        }
    }
    struct S1
    {
        C1 c1;
        this (C0 c)
        {
            c1 = new C1(c);
        }
    }
    class C1
    {
        C0 c0;
        this(C0 c)
        {
            c0 = c;
        }
    }
    immutable x = cast(immutable)r;

    auto r()
    {
        C0 c0 = new C0();
        return c0;
    }
}

alias test3 = Test3!();

// From https://issues.dlang.org/show_bug.cgi?id=22114

template Test4()
{
    public class Test1(T)
    {
        private Test2!T val;

        this()
        {
            val = new Test2!T(this);
        }

        private class Test2(T)
        {
            private Test1!(T) m_source;

            this(Test1!T source)
            {
                m_source = source;
            }
        }
    }

    public class Demo
    {
        auto val = new Test1!int();
    }
}

alias test4 = Test4!();

// ditto

template Test5()
{
    public @nogc class TestA(T)
    {
        private TestB!T valA;
        private TestB!T valB;
        this()
        {
            valB = valA = new TestB!T(this);
        }

        private @nogc class TestB(T)
        {
            private TestA!(T) m_source;

            this(TestA!T source)
            {
                m_source = source;
            }
        }
    }

    public class Demo
    {
        auto val = new TestA!int();
    }
}

alias test5 = Test5!();
