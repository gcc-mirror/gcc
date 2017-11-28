// PR c++/66515
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-return-type" }

#include <initializer_list>

struct type_t { };

type_t &
get ()
{
  std::initializer_list<type_t>{ { get () } };
}
