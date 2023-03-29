// PR c++/108437
// { dg-do run { target c++23 } }

struct S { static int &operator[] (int x) { static int a[2]; return a[x]; } };
struct U { static int &operator[] (int x, int y, int z) { static int a[2]; return a[x + y + z]; } };
struct V { static int &operator[] () { static int a; return a; } };

template <class T, class W, class X> void
foo ()
{
  S s;
  s[0]++;
  T t;
  t[0]++;
  U u;
  u[0, 0, 0]++;
  V v;
  v[]++;
  W w;
  w[0, 0, 0]++;
  X x;
  x[]++;
}

int
main ()
{
  S::operator[] (0) = 1;
  U::operator[] (0, 0, 0) = 2;
  V::operator[] () = 3;
  foo <S, U, V> ();
  if (S::operator[] (0) != 3 || U::operator[] (0, 0, 0) != 4 || V::operator[] () != 5)
    __builtin_abort ();
}
