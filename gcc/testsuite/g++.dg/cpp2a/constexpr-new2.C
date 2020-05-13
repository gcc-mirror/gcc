// P0784R7
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

template <int N>
constexpr bool
foo (const char (&x)[N])
{
  int **p = new int *[N];
  for (int i = 0; i < N; i++)
    p[i] = new int (x[i]);
  for (int i = 0; i < N; i++)
    if (*p[i] != x[i])
      return false;
  for (int i = 0; i < N; ++i)
    delete p[i];
  delete[] p;
  return true;
}

constexpr bool a = foo ("foobar");
static_assert (a);
