// PR c++/98800
// { dg-do compile { target c++11 } }

template <bool> struct enable_if_t {};
struct tmp {
  template <class>   constexpr bool is_integral();
  template <class E> static auto func() -> enable_if_t<is_integral<E>()>; // { dg-error "without object" }
};
template <class> constexpr bool tmp::is_integral() { return true; }
int main() { tmp::func<int>(); } // { dg-error "no match" }
