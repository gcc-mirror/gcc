// PR c++/66988
// { dg-options "-std=c++1z -fconcepts" }

#include <type_traits>

template <template <class> class T, class U>
concept bool _Valid = requires { typename T<U>; };

template <class T>
using __t = typename T::type;

template <class T>
struct __has_type : std::false_type { };

template <class T>
  requires _Valid<__t, T>
struct __has_type<T> : std::true_type { };

static_assert(!__has_type<int>(), "");
