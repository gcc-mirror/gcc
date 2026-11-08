// PR c++/127232
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

// Check that reflections of locals/parameters refer to the current frame in
// recursive calls.
consteval bool
foo (int n, int *outer)
{
  int i = n;
  if (&extract<int &> (^^i) != &i || &extract<int &> (^^n) != &n)
    return false;
  if (outer && &extract<int &> (^^i) == outer)
    return false;
  int &ri = i;
  if (&extract<int &> (^^ri) != &i)
    return false;
  return n == 0 || foo (n - 1, &i);
}
static_assert (foo (2, nullptr));
