// PR c++/67846
// { dg-do compile { target c++11 } }

class A
{
  void foo ()
  {
    [=] { return foo; };  // { dg-error "cannot convert" }
  }
  void bar () const;
  void bar ()
  {
    [=] { return bar; };  // { dg-error "unable to deduce" }
  }
};
