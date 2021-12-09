// EXTRA_CPP_SOURCES: cpp22287.cpp

extern(C++):

class X
{
public:
    ~this();
    int i;
}

class Y : X
{
}

class A
{
    ~this();
    int f1() const;

    int i;
}

interface I
{
    int f2() const;
    X f4();
}

class B : A, I
{
    override int f1() const;
    override int f2() const;
    int f3() const;
    override X f4();
}

class C : B
{
    override int f1() const;
    override int f2() const;
    override int f3() const;
    override Y f4();
}

version(Windows)
{
class D : B
{
    override int f1() const
    {
        return i + 41;
    }

    override int f2() const
    {
        return i + 42;
    }

    override int f3() const
    {
        return i + 43;
    }

    override Y f4()
    {
        Y r = new Y;
        r.i = i + 44;
        return r;
    }
}

mixin template MixinE()
{
    override int f1() const
    {
        return i + 51;
    }

    override int f2() const
    {
        return i + 52;
    }

    override int f3() const
    {
        return i + 53;
    }

    override Y f4()
    {
        Y r = new Y;
        r.i = i + 54;
        return r;
    }
}

class E : B
{
    mixin MixinE;
}
}

I createIFromCPP(char type, int i);
B createBFromCPP(char type, int i);
C createCFromCPP(int i);
version(Windows)
{
D createDFromCPP(int i);
E createEFromCPP(int i);
}

I createIFromD(char type, int i)
{
    switch (type)
    {
    case 'B':
    {
        B b = new B();
        b.i = i;
        return b;
    }
    case 'C':
    {
        C c = new C();
        c.i = i;
        return c;
    }
    version(Windows)
    {
    case 'D':
    {
        D d = new D();
        d.i = i;
        return d;
    }
    case 'E':
    {
        E e = new E();
        e.i = i;
        return e;
    }
    }
    default:
        return null;
    }
}

B createBFromD(char type, int i)
{
    switch (type)
    {
    case 'B':
    {
        B b = new B();
        b.i = i;
        return b;
    }
    case 'C':
    {
        C c = new C();
        c.i = i;
        return c;
    }
    version(Windows)
    {
    case 'D':
    {
        D d = new D();
        d.i = i;
        return d;
    }
    case 'E':
    {
        E e = new E();
        e.i = i;
        return e;
    }
    }
    default:
        return null;
    }
}

C createCFromD(int i)
{
    C c = new C();
    c.i = i;
    return c;
}

version(Windows)
{
D createDFromD(int i)
{
    D d = new D();
    d.i = i;
    return d;
}

E createEFromD(int i)
{
    E e = new E();
    e.i = i;
    return e;
}
}

void runCPPTests();

extern(D) void main()
{
    {
        B b = new B();
        b.i = 100;
        assert(b.f1() == 121);
        assert(b.f2() == 122);
        assert(b.f3() == 123);
        assert(b.f4().i == 124);
    }
    {
        C c = new C();
        c.i = 100;
        assert(c.f1() == 131);
        assert(c.f2() == 132);
        assert(c.f3() == 133);
        assert(c.f4().i == 134);
    }
    version(Windows)
    {
    {
        D d = new D();
        d.i = 100;
        assert(d.f1() == 141);
        assert(d.f2() == 142);
        assert(d.f3() == 143);
        assert(d.f4().i == 144);
    }
    {
        E e = new E();
        e.i = 100;
        assert(e.f1() == 151);
        assert(e.f2() == 152);
        assert(e.f3() == 153);
        assert(e.f4().i == 154);
    }
    }
    {
        I i = createIFromCPP('B', 100);
        assert(i.f2() == 122);
        assert(i.f4().i == 124);
    }
    {
        I i = createIFromCPP('C', 100);
        assert(i.f2() == 132);
        assert(i.f4().i == 134);
    }
    version(Windows)
    {
    {
        I i = createIFromCPP('D', 100);
        assert(i.f2() == 142);
        assert(i.f4().i == 144);
    }
    {
        I i = createIFromCPP('E', 100);
        assert(i.f2() == 152);
        assert(i.f4().i == 154);
    }
    }
    {
        B b = createBFromCPP('B', 100);
        assert(b.f1() == 121);
        assert(b.f2() == 122);
        assert(b.f3() == 123);
        assert(b.f4().i == 124);
    }
    {
        B b = createBFromCPP('C', 100);
        assert(b.f1() == 131);
        assert(b.f2() == 132);
        assert(b.f3() == 133);
        assert(b.f4().i == 134);
    }
    version(Windows)
    {
    {
        B b = createBFromCPP('D', 100);
        assert(b.f1() == 141);
        assert(b.f2() == 142);
        assert(b.f3() == 143);
        assert(b.f4().i == 144);
    }
    {
        B b = createBFromCPP('E', 100);
        assert(b.f1() == 151);
        assert(b.f2() == 152);
        assert(b.f3() == 153);
        assert(b.f4().i == 154);
    }
    }
    {
        C c = createCFromCPP(100);
        assert(c.f1() == 131);
        assert(c.f2() == 132);
        assert(c.f3() == 133);
        assert(c.f4().i == 134);
    }
    version(Windows)
    {
    {
        D d = createDFromCPP(100);
        assert(d.f1() == 141);
        assert(d.f2() == 142);
        assert(d.f3() == 143);
        assert(d.f4().i == 144);
    }
    {
        E e = createEFromCPP(100);
        assert(e.f1() == 151);
        assert(e.f2() == 152);
        assert(e.f3() == 153);
        assert(e.f4().i == 154);
    }
    }
    runCPPTests();
}
