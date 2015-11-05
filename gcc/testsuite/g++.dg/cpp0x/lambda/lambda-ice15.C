// PR c++/67846
// { dg-do compile { target c++11 } }

class A
{
  void foo ()
  {
    [=] { return foo; };  // { dg-error "invalid use of member function" }
  }
};
