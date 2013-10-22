// PR c++/42277
// { dg-options -std=c++11 }

struct S { int s; };
template <int N>
void foo ()
{
  S s;
  decltype (s.s) i;
}
