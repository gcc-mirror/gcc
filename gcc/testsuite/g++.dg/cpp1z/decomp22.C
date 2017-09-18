// PR c++/79205
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <unsigned long, typename...> struct B;
template <unsigned long I, typename H> struct B<I, H> { int b; };
template <typename... E> struct C { B<0, E...> c; C (C &) = default; C (C &&); };
template <typename> struct tuple_size;
template <> struct tuple_size<C<int>> { static constexpr int value = 1; };
template <int, typename> struct tuple_element;
template <typename H, typename... T>
struct tuple_element<0, C<H, T...>> { typedef int type; };
template <int, typename... E>
int &&get (C<E...> &&);

int
foo (C<int> t)
{
  auto[x0] = t;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return x0;
}
