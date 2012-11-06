// DR 1402
// { dg-options -std=c++11 }

template <class T> T&& move(T& t);

struct A
{
  A(const A&);
};

struct B
{
  B(B&&);
};

struct C
{
  A a;
  B b;
};

extern C c1;
C c2(move(c1));
