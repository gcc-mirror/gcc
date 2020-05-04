// PR c++/94938

template <typename T, int N> int
foo (T t, int i)
{
  int m1 = 10 / t;
  int m2 = 10 / i;
  int m3 = 10 / (sizeof(T) - sizeof(int)); // { dg-warning "division by" }
  int m4 = 10 / N; // { dg-warning "division by" }
  return m1 + m2 + m3 + m4;
}

void
f ()
{
  foo<int, 0>(0, 0);
}
