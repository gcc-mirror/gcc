// PR c++/57510
// { dg-do run { target c++11 } }

#include <initializer_list>

struct counter
{
  static int n;

  counter() { ++n; }
  counter(const counter&) { ++n; }
  ~counter() { --n; }
};

int counter::n = 0;

struct X
{
    X () { if (counter::n > 1) throw 1; }

    counter c;
};

int main ()
{
  try
  {
    auto x = { X{}, X{} };
  }
  catch (...)
  {
    if ( counter::n != 0 )
      throw;
  }
}
