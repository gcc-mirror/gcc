// PR c++/70869
// { dg-do run { target c++11 } }

#include <initializer_list>

struct A
{
  int f () { return 1; }
  int g () { return 2; }
  int h () { return 3; }
};

int
main ()
{
  int cnt = 0;
  for (const auto &m : { &A::f, &A::g, &A::h })
    {
      A a;
      if ((a.*m) () != ++cnt)
	__builtin_abort ();
    }
  if (cnt != 3)
    __builtin_abort ();
}
