// PR c++/95143
// { dg-do compile { target c++11 } }

struct false_type {
  static constexpr bool value = false;
};

struct true_type{
  static constexpr bool value = true;
};

template<class T>
T&& declval() noexcept;

template<typename T, typename U, typename = U>
struct is_static_castable : false_type
{};
template<typename T, typename U>
struct is_static_castable<T, U, decltype(static_cast<U>(declval<T>()))> : true_type
{};

class Base { };
struct A { };
class B: public Base { };

int main()
{
  constexpr auto canCast = is_static_castable<A, B>::value;
  static_assert(!canCast, "");
  constexpr auto canCast2 = is_static_castable<A, A>::value;
  static_assert(canCast2, "");
}
