// PR c++/21123

struct A
{
  A( const A &a);
  const A& operator=( const A& a);
};

struct B
{
  virtual A f();
};

struct C : virtual B
{
  virtual A f();
  A a;
};

A C::f() 
{
  return a;
}
