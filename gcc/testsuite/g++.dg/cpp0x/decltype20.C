// PR c++/42277
// { dg-do compile { target c++11 } }

struct S { int s; };
template <int N>
void foo ()
{
  S s;
  decltype (s.s) i;
}
