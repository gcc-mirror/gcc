// { dg-do run  }
// Make sure we can initialize complex (MI ambiguous) vtables.

extern "C" int printf(const char *, ...);

struct a
{
    virtual void f() = 0;
};

struct b
{
    virtual void g() { };
};

struct c: public a, public b
{
    virtual void f();
    virtual void g();
};

void c::f()
{
    printf("c::f()\n");
}

void c::g()
{
    printf("c::g()\n");
}

struct e: public b
{
};

struct h
{
};

struct d: public c, public e, public h
{
    virtual void f();
    virtual void g();
};
void d::f()
{
    printf("d::f()\n");
}

void d::g()
{
    printf("d::g()\n");
}

int main(int argc, char* argv[])
{
    a* tmp = new d;
    tmp->f();
    return 0;
}
