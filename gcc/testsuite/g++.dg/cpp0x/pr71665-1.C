// PR c++/71665
// { dg-do compile { target c++11 } }

class A 
{
  int f ();
  enum { a = f }; // { dg-error "enumerator" }
};
