// PR c++/79960
// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof(0));

template<typename T> struct tuple_size;

template<typename T, size_t U = tuple_size<T>::value>
  using __has_tuple_size = T;

template<typename T> struct tuple_size<const __has_tuple_size<T>> {
  static constexpr size_t value = tuple_size<T>::value;
};

template<typename T> struct tuple_size<volatile __has_tuple_size<T>> {
  static constexpr size_t value = tuple_size<T>::value;
};

template<typename T> struct tuple_size<const __has_tuple_size<volatile T>> {
  static constexpr size_t value = tuple_size<T>::value;
};

template<typename... T> struct tuple { };
template<typename... T> struct tuple_size<tuple<T...>> {
  static constexpr size_t value = sizeof...(T);
};

static_assert( tuple_size<const tuple<>>::value == 0, "" );  // OK
static_assert( tuple_size<volatile tuple<>>::value == 0, "" ); // OK
static_assert( tuple_size<const volatile tuple<>>::value == 0, "" ); // FAIL
