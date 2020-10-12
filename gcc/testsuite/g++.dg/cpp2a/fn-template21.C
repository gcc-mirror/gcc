// PR c++/97010
// { dg-do compile { target c++20 } }

namespace M {
  struct S { int x; };
  S foo ();

  template<typename>
  void get (S);
}

template<typename T>
void bar (const T& t)
{
  get<int>(t);
}

int
main ()
{
  auto a = M::foo ();
  get<int>(a);
  bar (a);
}
