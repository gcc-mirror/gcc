// PR c++/117107
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename, typename>
constexpr bool is_same = false;	// { dg-warning "variable template" "" { target c++11_down } }
template <typename T>
constexpr bool is_same<T, T> = true; // { dg-warning "variable template" "" { target c++11_down } }

struct tuple {
  template <unsigned long I>
  void check_tuple_like() {
    tuple t;
    auto [v, r] = t; // { dg-warning "structured bindings" "" { target c++14_down } }
    (void)[v, r] {   // { dg-warning "captured structured" "" { target c++17_down } }
        decltype(v) x;
    };
  }
  int a = 0;
  int &b = a;
};
