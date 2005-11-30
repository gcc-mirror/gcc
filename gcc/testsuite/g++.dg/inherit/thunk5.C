// PR c++/21123

struct A
{
  A(const A &a);
  const A& operator=(const A& a);
};

struct B
{
  virtual A f(A);
};

struct C : virtual B
{
  virtual A f(A);
};

A C::f(A a) 
{
  return a;
}
