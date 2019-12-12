// PR c++/87547
// { dg-do run }

#include <typeinfo>

struct S { unsigned int a : 4; unsigned int b : 12; int c; unsigned long d : 8; } s;

int
main ()
{
  if (typeid (s.a) != typeid (unsigned int)
      || typeid (s.b) != typeid (unsigned int)
      || typeid (s.c) != typeid (int)
      || typeid (s.d) != typeid (unsigned long))
    __builtin_abort ();
}
