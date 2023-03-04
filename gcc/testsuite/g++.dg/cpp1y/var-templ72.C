// PR c++/108550
// { dg-do compile { target c++14 } }

template<class T>
struct is_pointer
{
  static constexpr bool value = false;
};

template<class T, T T1>
struct integral_constant
{
  static constexpr T value = T1;
};

template<typename T>
using PTR_P = is_pointer<T>;

template <class Tp>
constexpr auto is_pointer_v = PTR_P<Tp>::value;

template <class Tp, int = 0>
integral_constant<bool, is_pointer_v<Tp>> Wrap1();

int main() {
  static_assert(!decltype(Wrap1<int>())::value, "");
}
