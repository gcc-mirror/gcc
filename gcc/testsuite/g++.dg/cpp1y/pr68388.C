// { dg-do compile { target c++14 } }

namespace std {
typedef int size_t;
template <std::size_t... Indexes>
struct index_sequence {};
}

template <class...> class Tuple {};

template <std::size_t I, class TFirst>
auto get(Tuple<TFirst>) {
  return TFirst();
}

template <std::size_t I, class Functor, class T1, class T2>
auto apply_impl(Functor f, T1 t1, T2 t2) {
  return f(get<I>(t1), get<I>(t2));
}

template <std::size_t... Indexes, class Functor, class T1, class T2>        
auto map_impl(std::index_sequence<Indexes...>, Functor f, T1 t1, T2 t2) {
  Tuple<decltype(apply_impl<Indexes>(f, t1, t2))...>();
}

template <class Functor, class T1, class T2>
auto map_impl(Functor f, T1 t1, T2 t2) {
  map_impl(std::index_sequence<0>(), f, t1, t2);
}
                                 
struct Less {                                                
  template <class Lhs, class Rhs>                     
  auto operator()(Lhs lhs, Rhs rhs) -> decltype(lhs < rhs) {
    return lhs < rhs;
  }
};
                                                                     
int main() {
  auto t1 = Tuple<int>();
  auto t2 = Tuple<int>();
  map_impl(Less(), t1, t2);
}
