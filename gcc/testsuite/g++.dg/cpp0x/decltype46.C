// PR c++/55558
// { dg-do compile { target c++11 } }

struct A
{
  static int member;
};

template<typename T> void foobar ()
{
  typedef decltype (A::member) myType;
}
