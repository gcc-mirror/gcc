// Source: Neil Booth, from PR #111.

class A
{
public :
     int i;
};

class B : virtual public A
{
};

class C : virtual public A
{
};

class D : public B, public C
{
public :
     int f(void);
     int g(void);
};

int D::f(void)
{
     return B::i;
}

int D::g(void)
{
     return this->B::i;
}

D d;
extern "C" void abort (void);

int main(void)
{
    d.C::i=325;

    if (d.f() != d.B::i || d.f() != d.g())
      abort ();

    return 0;
}
