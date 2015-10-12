// PR c++/58566
// { dg-do compile { target c++11 } }

struct A
{
  int foo()
  {
    [this]{ return foo; }; // { dg-error "invalid use of member function|cannot convert" }
  }
};
