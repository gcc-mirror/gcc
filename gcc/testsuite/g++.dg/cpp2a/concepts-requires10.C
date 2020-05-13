// PR c++/66988
// { dg-do compile { target c++20 } }

template<bool B>
struct bool_constant {
  static constexpr bool value = B;
  constexpr operator bool() const { return value; }
};

using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

template <template <class> class T, class U>
concept _Valid = requires { typename T<U>; };

template <class T>
using nested_type = typename T::type;

template <class T>
struct has_nested_type : false_type { };

template <class T>
  requires _Valid<nested_type, T>
struct has_nested_type<T> : true_type { };

struct Nested
{
  using type = int;
};

static_assert(!has_nested_type<int>());
static_assert(has_nested_type<Nested>());
