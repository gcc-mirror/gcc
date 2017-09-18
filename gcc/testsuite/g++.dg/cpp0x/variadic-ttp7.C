// PR c++/81215
// { dg-do compile { target c++11 } }

template<typename U> struct X { };
template<typename T, typename U = void> struct set { };

template <typename V, template <typename...> class C>
void bar (const X<C<V>>&)
{
}

void
foo (X<set<int>>& x)
{
  bar (x);
}
