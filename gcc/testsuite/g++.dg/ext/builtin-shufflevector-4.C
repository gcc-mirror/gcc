// PR c++/106001
// { dg-do compile }

typedef int V __attribute__((vector_size (2 * sizeof (int))));

template <int>
void
foo ()
{
  V v = {};
  v = __builtin_shufflevector (v, v, static_cast<char>(1), static_cast<char>(0));
}

void
bar ()
{
  foo <0> ();
}
