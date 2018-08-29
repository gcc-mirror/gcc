// PR c++/84454
// { dg-do compile { target c++11 } }

template<class F, class... A>
void
g(F&&, A&&...)
{}

template<class... A>
auto
h(A&&... a) -> decltype(g(0, g<decltype(a)>(a)...))
{
  g(a...);			// { dg-error "no match" }
}

int
main()
{
  h();
}
