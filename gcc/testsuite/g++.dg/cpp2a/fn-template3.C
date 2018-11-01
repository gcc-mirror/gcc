// P0846R0
// { dg-do run }
// { dg-options "-std=c++2a" }

void g();
void e();
void e(int);
void e(int, int);

namespace N {
  struct A { };
  template <class T> int f(T) { return 1; }
  template <class T> int g(T) { return 2; }
  template <class T> int e(T) { return 3; }
}

int
main ()
{
  int v = e<N::A>(N::A());
  if (v != 3)
    __builtin_abort ();
  int x = f<N::A>(N::A());
  if (x != 1)
    __builtin_abort ();
  int y = g<N::A>(N::A());
  if (y != 2)
    __builtin_abort ();
}
