// PR 41377
// { dg-do compile }
// { dg-options "-O3" }

struct A
{
  bool foo(int*) const;
} a;

struct B {};

struct B1 : B
{
  bool (A::*pmf)(int*) const;
  const A* pa;

  B1() : pmf(&A::foo), pa(&a) {}
  bool operator()() const { return (pa->*pmf)(new int); }
};

struct B2 : B
{
  B1 b1;

  B2(const B1& _b1) : b1(_b1) {}
  bool operator()() const { return b1(); }
};

template<int> struct C
{
  void bar(B2 b2) { while (b2()) ; }
  C() { bar(B2(B1())); }
};

void baz(int i)
{
  switch(i)
  {
    case 0: new C<0>;
    case 1: new C<1>;
    case 2: new C<2>;
  }
}
