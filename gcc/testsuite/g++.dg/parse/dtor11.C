// PR c++/28606
// { dg-do compile }

struct A
{
  ~A A();     // { dg-error "destructor" }
};

struct B
{
  A::~B B();  // { dg-error "as member of|as a type" }
};
