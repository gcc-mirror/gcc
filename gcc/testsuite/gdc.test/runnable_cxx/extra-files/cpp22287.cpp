#include <assert.h>

class X
{
public:
    virtual ~X();
    int i;
};

X::~X()
{
}

class Y : public X
{
};

class A
{
public:
    virtual ~A();
    virtual int f1() const;

    int i;
};

class I
{
public:
    virtual int f2() const = 0;
    virtual X *f4() = 0;
};

class B : public A, public I
{
public:
    virtual int f1() const;
    virtual int f2() const;
    virtual int f3() const;
    virtual X *f4();
};

class C : public B
{
public:
    virtual int f1() const;
    virtual int f2() const;
    virtual int f3() const;
    virtual Y *f4();
};

#ifdef _WIN32
class D : public B
{
public:
    virtual int f1() const;
    virtual int f2() const;
    virtual int f3() const;
    virtual Y *f4();
};

class E : public B
{
public:
    virtual int f1() const;
    virtual int f2() const;
    virtual int f3() const;
    virtual Y *f4();
};
#endif

A::~A()
{
}

int A::f1() const
{
    return i + 11;
}

int B::f1() const
{
    return i + 21;
}

int B::f2() const
{
    return i + 22;
}

int B::f3() const
{
    return i + 23;
}

X *B::f4()
{
    X *r = new X;
    r->i = i + 24;
    return r;
}

int C::f1() const
{
    return i + 31;
}

int C::f2() const
{
    return i + 32;
}

int C::f3() const
{
    return i + 33;
}

Y *C::f4()
{
    Y *r = new Y;
    r->i = i + 34;
    return r;
}

I *createIFromCPP(char type, int i)
{
    switch (type)
    {
    case 'B':
    {
        B *b = new B();
        b->i = i;
        return b;
    }
    case 'C':
    {
        C *c = new C();
        c->i = i;
        return c;
    }
#ifdef _WIN32
    case 'D':
    {
        D *d = new D();
        d->i = i;
        return d;
    }
    case 'E':
    {
        E *e = new E();
        e->i = i;
        return e;
    }
#endif
    default:
        return 0;
    }
}

B *createBFromCPP(char type, int i)
{
    switch (type)
    {
    case 'B':
    {
        B *b = new B();
        b->i = i;
        return b;
    }
    case 'C':
    {
        C *c = new C();
        c->i = i;
        return c;
    }
#ifdef _WIN32
    case 'D':
    {
        D *d = new D();
        d->i = i;
        return d;
    }
    case 'E':
    {
        E *e = new E();
        e->i = i;
        return e;
    }
#endif
    default:
        return 0;
    }
}

C *createCFromCPP(int i)
{
    C *c = new C();
    c->i = i;
    return c;
}

#ifdef _WIN32
D *createDFromCPP(int i)
{
    D *d = new D();
    d->i = i;
    return d;
}

E *createEFromCPP(int i)
{
    E *e = new E();
    e->i = i;
    return e;
}
#endif

I *createIFromD(char type, int i);
B *createBFromD(char type, int i);
C *createCFromD(int i);
#ifdef _WIN32
D *createDFromD(int i);
E *createEFromD(int i);
#endif

void runCPPTests()
{
    {
        B *b = new B();
        b->i = 100;
        assert(b->f1() == 121);
        assert(b->f2() == 122);
        assert(b->f3() == 123);
        assert(b->f4()->i == 124);
    }
    {
        C *c = new C();
        c->i = 100;
        assert(c->f1() == 131);
        assert(c->f2() == 132);
        assert(c->f3() == 133);
        assert(c->f4()->i == 134);
    }
#ifdef _WIN32
    {
        D *d = new D();
        d->i = 100;
        assert(d->f1() == 141);
        assert(d->f2() == 142);
        assert(d->f3() == 143);
        assert(d->f4()->i == 144);
    }
    {
        E *e = new E();
        e->i = 100;
        assert(e->f1() == 151);
        assert(e->f2() == 152);
        assert(e->f3() == 153);
        assert(e->f4()->i == 154);
    }
#endif
    {
        I *i = createIFromD('B', 100);
        assert(i->f2() == 122);
        assert(i->f4()->i == 124);
    }
    {
        I *i = createIFromD('C', 100);
        assert(i->f2() == 132);
        assert(i->f4()->i == 134);
    }
#ifdef _WIN32
    {
        I *i = createIFromD('D', 100);
        assert(i->f2() == 142);
        assert(i->f4()->i == 144);
    }
    {
        I *i = createIFromD('E', 100);
        assert(i->f2() == 152);
        assert(i->f4()->i == 154);
    }
#endif
    {
        B *b = createBFromD('B', 100);
        assert(b->f1() == 121);
        assert(b->f2() == 122);
        assert(b->f3() == 123);
        assert(b->f4()->i == 124);
    }
    {
        B *b = createBFromD('C', 100);
        assert(b->f1() == 131);
        assert(b->f2() == 132);
        assert(b->f3() == 133);
        assert(b->f4()->i == 134);
    }
#ifdef _WIN32
    {
        B *b = createBFromD('D', 100);
        assert(b->f1() == 141);
        assert(b->f2() == 142);
        assert(b->f3() == 143);
        assert(b->f4()->i == 144);
    }
    {
        B *b = createBFromD('E', 100);
        assert(b->f1() == 151);
        assert(b->f2() == 152);
        assert(b->f3() == 153);
        assert(b->f4()->i == 154);
    }
#endif
    {
        C *c = createCFromD(100);
        assert(c->f1() == 131);
        assert(c->f2() == 132);
        assert(c->f3() == 133);
        assert(c->f4()->i == 134);
    }
#ifdef _WIN32
    {
        D *d = createDFromD(100);
        assert(d->f1() == 141);
        assert(d->f2() == 142);
        assert(d->f3() == 143);
        assert(d->f4()->i == 144);
    }
    {
        E *e = createEFromD(100);
        assert(e->f1() == 151);
        assert(e->f2() == 152);
        assert(e->f3() == 153);
        assert(e->f4()->i == 154);
    }
#endif
}
