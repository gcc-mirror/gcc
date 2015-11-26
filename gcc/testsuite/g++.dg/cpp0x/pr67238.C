// { dg-do compile { target c++11 } }
// { dg-options "-g" }

template<class F, class... A>
void
g(F&&, A&&...)
{}

template<class... A>
auto
h(A&&... a) -> decltype(g(0, g<decltype(a)>(a)...))
{
  return g([]{}, a...);
}

int
main()
{
  h();
}
