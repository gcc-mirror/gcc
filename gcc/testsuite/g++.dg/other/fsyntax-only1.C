// PR c++/80831
// { dg-options -fsyntax-only }
// { dg-do compile { target c++11 } }

class A
{
public:
    virtual ~A() { }
};

class B { };

class C : public A { };

template<class J>
class D : public C
{
public:
    D() { }
    ~D() { }
};

class E
{
public:
    static E& p();
    B q();
    template<class J>
    B q(void (J::*r)())
    {
        new D<J>();
        return q();
    }
};

void t()
{
  class F
  {
  public:
    virtual void s() { }
  };
  E& x = E::p();
  B y = x.q(&F::s);
}
