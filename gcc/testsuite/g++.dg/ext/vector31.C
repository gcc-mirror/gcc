// PR c++/71871
// { dg-do compile }

typedef unsigned int V __attribute__ ((__vector_size__ (32)));

template <int N>
void
foo (V *x)
{
  V a = *x;
  a = a ? a : -1;
  *x = a;
}

template <typename T>
void
bar (T *x)
{
  T a = *x;
  a = a ? a : -1;
  *x = a;
}

void
test (V *x, V *y)
{
  foo<0> (x);
  bar<V> (y);
}
