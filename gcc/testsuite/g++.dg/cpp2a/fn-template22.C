// PR c++/97010
// { dg-do compile { target c++20 } }

namespace M {
  struct S { int x; };
  S foo ();

// Test not-found-by-ADL scenario.
// template<typename>
// void get (S);
}

template<typename T>
void bar (const T& t)
{
  get<int>(t); // { dg-error ".get. was not declared in this scope" }
}

int
main ()
{
  auto a = M::foo ();
  get<int>(a); // { dg-error ".get. was not declared in this scope" }
  bar (a);
}
