// { dg-do compile { target c++11 } }

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };

template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

enum E1 : unsigned { };
enum E2 : char { };
enum class E3 { };
enum class E4 : unsigned char { c = 1 };
enum class E5 : int { a = -1, b = 1 };
enum class E6 : long { c = __LONG_MAX__ };

template<typename T, typename U,
	 typename V = __underlying_type(T)>
  struct test
  {
    static_assert(is_same<U, V>::value, "Error");
  };

template class test<E1, unsigned>;
template class test<E2, char>;
template class test<E3, int>;
template class test<E4, unsigned char>;
template class test<E5, int>;
template class test<E6, long>;
