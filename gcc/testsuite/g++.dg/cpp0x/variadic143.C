// PR c++/56782
// { dg-options -std=c++0x }

template<class T>
T&& declval();

struct is_convertible_impl {
  template<class T>
  static void sink(T);

  template<class T, class U, class = decltype(sink<U>(declval<T>()))>
  static auto test(int) -> char;

  template<class, class>
  static auto test(...) -> char(&)[2];
};

template<class T, class U>
struct is_convertible : is_convertible_impl
{
  static const bool value = sizeof(test<T, U>(0)) == 1;
};

template<bool, class>
struct enable_if {};

template<class T>
struct enable_if<true, T> { typedef T type; };

template<bool, class If, class Else>
struct conditional { typedef If type; };

template<class If, class Else>
struct conditional<false, If, Else> { typedef Else type; };

template<class...>
struct and_;

template<>
struct and_<>
{
  static const bool value = true;
};

template<class P>
struct and_<P> : P
{
};

template<class P1, class P2>
struct and_<P1, P2> : conditional<P1::value, P2, P1>::type
{
};

template<class... T>
struct Tuple {
  template<class... U,
	   class = typename enable_if<and_<is_convertible<U, T>... >::value, int>::type
	   >
  Tuple(U&&...){}
};

static_assert(is_convertible<Tuple<>, Tuple<>>::value, "Ouch"); //#1
