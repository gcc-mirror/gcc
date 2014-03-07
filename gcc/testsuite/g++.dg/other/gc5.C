// PR c++/51852
// { dg-do compile { target c++11 } }
// { dg-options "--param ggc-min-heapsize=0 --param ggc-min-expand=0" }

template <typename, typename>
class transformed {};

template <class R, class F>
transformed<F, R> transform (R r, F f);

template <typename, typename>
class joined {};

template <typename T, typename U>
joined<T, U> join (T t, U u);

template <typename T, typename U, typename V, typename... Rest>
auto join (T t, U u, V v, Rest... rest) -> decltype (join (join (t, u), v, rest...));

template <typename F, typename... Rs>
auto polymorphic_transform (F f, Rs... rs) -> decltype (join (transform(rs, f)...));

int
main ()
{
  polymorphic_transform (0, 0, 0);
}
