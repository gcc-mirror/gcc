// PR c++/67021
// { dg-do compile { target c++11 } }

template<typename> struct Dummy;
template<> struct Dummy<int> {};

template <class...>
struct all_same { static constexpr bool value = true; };
template <class T, class...Rest>
struct all_same<T, T, Rest...> : all_same<T, Rest...> {};
template <class T, class U, class...Rest>
struct all_same<T, U, Rest...> { static constexpr bool value = false; };

template <class R>
using ValueType = int;

template <class I>
constexpr bool A(I i) {
  return all_same<ValueType<I>, ValueType<decltype(i++)>>::value;
}

int main() {
  static_assert(A(42), "");
}
