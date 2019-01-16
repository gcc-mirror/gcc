// P0846R0
// { dg-do run }
// { dg-options "-std=c++2a" }

int g() { return 11; }
int e() { return 12; }
int e(int) { return 13; }
int e(int, int) { return 14; }

namespace N {
  struct A { };
  template <class T> int f(T) { return 1; }
  template <class T> int g(T) { return 2; }
  template <class T> int e(T) { return 3; }
}

int
main ()
{
  int v = e(1);
  if (v != 13)
    __builtin_abort ();
  int x = e(1, 2);
  if (x != 14)
    __builtin_abort ();
  int y = g();
  if (y != 11)
    __builtin_abort ();
  int z = e();
  if (z != 12)
    __builtin_abort ();
}
