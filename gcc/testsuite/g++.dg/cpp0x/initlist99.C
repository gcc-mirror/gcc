// PR c++/85092
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A
{
  A (std::initializer_list<char>);
};

A f ();

A a { f () };
