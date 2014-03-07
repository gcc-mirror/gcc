// Positive test for C++11 unrestricted unions
// { dg-do compile { target c++11 } }

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
