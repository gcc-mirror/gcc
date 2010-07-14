// Positive test for C++0x unrestricted unions
// { dg-options -std=c++0x }

struct A
{
  A();
  A(const A&);
  ~A();
};

union B
{
  A a;
  B();
  B(const B&);
  ~B();
};

B b;
B b2(b);

struct C
{
  union
  {
    A a;
  };
  C();
  C(const C&);
  ~C();
};

C c;
C c2(c);
