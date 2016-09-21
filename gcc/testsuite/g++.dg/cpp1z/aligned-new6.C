// PR c++/77651
// { dg-do run { target { c++11 && c++14_down } } }
// { dg-options "-faligned-new -W -Wall -Wno-aligned-new" }

struct alignas(64) A { int i; };

int
main ()
{
  A *p = new A;
  if (((__UINTPTR_TYPE__) p) % 64 != 0)
    __builtin_abort ();
  delete p;
}
