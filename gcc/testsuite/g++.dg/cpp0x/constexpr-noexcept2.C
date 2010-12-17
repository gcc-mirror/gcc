// { dg-options -std=c++0x }

template<class T>
constexpr T value(T t) { return t; }

template<class T>
struct is_funny {
  static constexpr bool value = false;
};

template<class T>
void eval() noexcept(value(is_funny<T>::value)) {}

constexpr bool ok = noexcept(eval<int>()); // line 12
