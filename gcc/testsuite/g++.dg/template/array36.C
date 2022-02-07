// Don't try to deduce array bounds from a dependent initializer.
// { dg-do compile { target c++11 } }

struct A { int i,j; };

template <class T> void f(T t)
{
  A ar[] = { t, t };
  static_assert (sizeof(ar)/sizeof(A) == 1, "");
}

int main()
{
  f(42);
}
