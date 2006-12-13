// { dg-do compile }

template <typename T>
struct V {
  T& operator[](int);
};

struct S {
  S operator +(int);
  template <typename T> T value();
};

template <typename T>
void R (T v)
{
  v[(S() + 0).template value<int>()][0] = 0;
}

int
main ()
{
  R(V<V<int> >());
}
