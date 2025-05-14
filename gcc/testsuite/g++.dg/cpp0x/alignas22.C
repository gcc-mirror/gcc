// PR c++/118787
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

template <typename T, int N>
void foo (T & alignas (N));		// { dg-warning "'alignas' on a type other than class" }
template <typename T, int N>
void bar (T (&)[N] alignas (N));	// { dg-warning "'alignas' on a type other than class" }
template <typename T, int N>
using U = T * alignas (N);		// { dg-warning "'alignas' on a type other than class" }
template <typename T, int N>
using V = T[N] alignas (N);		// { dg-warning "'alignas' on a type other than class" }

void
baz ()
{
  int x alignas (4) = 0;
  foo <int, 4> (x);
  int y alignas (4) [4];
  bar <int, 4> (y);
  U <int, 4> u;
  V <int, 4> v;
}
