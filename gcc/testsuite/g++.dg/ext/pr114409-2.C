// PR c++/114409
// { dg-do compile }
// { dg-options "-O2" }

template <typename T>
T
foo (T)
{
  static T t;
  return 42 - ++t;
}

template <typename T>
void
bar (T x)
{
  #pragma GCC novector
  while (T y = foo (x))
    ++y;
}

template <typename T>
void
baz (T x)
{
  #pragma GCC novector
  for (; T y = foo (x); )
    ++y;
}

void
qux ()
{
  bar (0);
  baz (0);
}
