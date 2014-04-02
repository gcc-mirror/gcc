// { dg-do compile { target c++11 } }

enum E1 : unsigned { E1_en = 1 };
enum E2 : char { E2_en = 1 };
enum class E3 { a = -1 };
enum class E4 : unsigned char { c = 1 };
enum class E5 : int { a = -1, b = 1 };
enum class E6 : long { c = __LONG_MAX__ };

template<typename T1, typename T2>
  struct is_same
  { static const bool value = false; };

template<typename T>
  struct is_same<T, T>
  { static const bool value = true; };

template<typename>
  struct underlying_type;

template<typename T, typename U>
  void
  test(T, U, typename underlying_type<T>::type);

template<typename T>
  struct underlying_type
  { typedef __underlying_type(T) type; };

template<typename T, typename U>
  void
  test(T, U, typename underlying_type<T>::type)
  {
    static_assert(is_same<typename underlying_type<T>::type, U>::value,
		  "Error");
  }

int main()
{
  test(E1::E1_en, unsigned(), 1);
  test(E2::E2_en, char(), 1);
  test(E3::a, int(), -1);
  test(E4::c, (unsigned char)(1), 1);
  test(E5::a, int(), -1);
  test(E6::c, long(), __LONG_MAX__);
}
