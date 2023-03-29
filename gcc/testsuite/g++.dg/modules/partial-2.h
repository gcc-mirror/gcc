template<class T> constexpr bool is_reference_v = false;
template<class T> constexpr bool is_reference_v<T&> = true;
template<class T> constexpr bool is_reference_v<T&&> = true;

struct A {
  template<class T> static constexpr bool is_reference_v = false;
};

template<class T> constexpr bool A::is_reference_v<T&> = true;
template<class T> constexpr bool A::is_reference_v<T&&> = true;

#if __cpp_concepts
namespace concepts {
  template<class T> bool is_reference_v;

  template<class T> requires __is_same(T, T&)
  constexpr bool is_reference_v<T> = true;

  template<class T> requires __is_same(T, T&&) && (!__is_same(T, T&))
  constexpr bool is_reference_v<T> = true;

  template<class T> requires (!__is_same(T, T&)) && (!__is_same(T, T&&))
  constexpr bool is_reference_v<T> = false;

  struct A {
    template<class T> static bool is_reference_v;
  };

  template<class T> requires __is_same(T, T&)
  constexpr bool A::is_reference_v<T> = true;

  template<class T> requires __is_same(T, T&&) && (!__is_same(T, T&))
  constexpr bool A::is_reference_v<T> = true;

  template<class T> requires (!__is_same(T, T&)) && (!__is_same(T, T&&))
  constexpr bool A::is_reference_v<T> = false;
}
#endif
