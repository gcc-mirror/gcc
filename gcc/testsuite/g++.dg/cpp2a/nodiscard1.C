// PR c++/105143
// { dg-do compile { target c++20 } }
// We used to crash here with "Error reporting routines re-entered".

template<class...> struct A { };

template<A V> using type = int;

template<A V> [[nodiscard]] type<V> get();

int main() {
  get<{}>(); // { dg-warning "nodiscard" }
}
