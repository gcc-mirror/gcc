
// https://issues.dlang.org/show_bug.cgi?id=3543

// merge with commontype.d?

void testi(bool bla)
{
    interface Root { }
    interface A : Root { }
    interface B : Root { }
    A a;
    B b;
    Root r = bla ? a : b;
    static assert(is(typeof(r) == Root));
    Root[] t = [a, b];
    static assert(is(typeof(t[0]) == Root));
}

void testc(bool bla)
{
    class Root { }
    class A : Root { }
    class B : Root { }
    A a;
    B b;
    Root r = bla ? a : b;
    static assert(is(typeof(r) == Root));
    Root[] t = [a, b];
    static assert(is(typeof(t[0]) == Root));
}

void teste(bool bla)
{
    interface Root { }
    interface Othe { }
    interface A : Root, Othe { }
    interface B : Root, Othe { }
    A a;
    B b;
    static assert(!__traits(compiles, bla ? a : b));
}

void testf(bool bla)
{
    interface Othe { }
    interface Root : Othe { }
    interface A : Root { }
    interface B : Othe { }
    A a;
    B b;
    Othe r = bla ? a : b;
}

void testg()
{
    interface A{}
    interface B{}

    interface C:A{}
    interface D:B,C{}

    interface E:B{}
    interface F:A,E{}

    D d;
    F f;
    static assert(!__traits(compiles, true ? d : f));
    static assert(!__traits(compiles, true ? f : d));
}

void testh()
{
    interface I {}
    class B {}
    class C : B, I {}
    class D : B {}
    C c;
    D d;
    auto b = true ? c : d;
}
