// PR c++/86669
// { dg-do run { target c++11 } }

#include <initializer_list>

struct A { };
struct S : virtual public A { S (); };
struct T : public S, virtual public A {};
int cnt;
void foo (int) { cnt++; }

S::S ()
{
  int e = 1, f = 2, g = 3, h = 4;

  for (auto k : { e, f, g, h })
    foo (k);
}

int
main ()
{
  S s;
  if (cnt != 4)
    __builtin_abort ();
  T t;
  if (cnt != 8)
    __builtin_abort ();
}
