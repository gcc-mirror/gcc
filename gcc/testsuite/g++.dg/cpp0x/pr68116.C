// PR c++/68116
// { dg-do compile { target c++11 } }

class C {
  void foo ();
  typedef void (C::*T) (int);
  static T b[];
};
C::T C::b[]
{
  T (&C::foo)
};
