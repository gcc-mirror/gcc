// { dg-do run  }
// prms-id: 10849

struct A
{
  int comm;
  A(int i) : comm(i) { }
};

struct S1 { char c1; };

struct B : public S1, public A
{
  B(int i) : A(i) { }
};

struct C : public A
{
  C(int i) : A(i) { }
};

struct D : public B, public C
{
  virtual int g() {
    int D::*pmd = (int C::*)&C::comm;
    return (this->*pmd) == 42;
  }
  D() : B(41), C(42) { }
} d;

int main() {
  if (! d.g())
    return 1;
}
