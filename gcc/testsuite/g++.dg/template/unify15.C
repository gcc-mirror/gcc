// { dg-do run }
#include <cassert>

template <typename T, int N>
int bar (T (&) [N]) { return 0; }

template <typename T, int N>
int bar (const T (&) [N]) { return 1; }

int
main ()
{
  const int s[2] = { 0 };
  assert (bar (s) == 1);
}
