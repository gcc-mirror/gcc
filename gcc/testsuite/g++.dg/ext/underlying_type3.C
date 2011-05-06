// { dg-do compile }
// { dg-options "-std=c++0x" }

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

__underlying_type(E1) i1 = __INT_MAX__ * 2U + 1;
__underlying_type(E2) i2 = (char(-1) < 0
			    ? __SCHAR_MAX__
			    : __SCHAR_MAX__ * 2U + 1);
__underlying_type(E3) i3 = __INT_MAX__;
__underlying_type(E4) i4 = __SCHAR_MAX__ * 2U + 1;
__underlying_type(E5) i5 = int(E5::b);
__underlying_type(E6) i6 = __LONG_MAX__;

static_assert(is_same<__underlying_type(E1), unsigned>::value, "Error");
static_assert(is_same<__underlying_type(E2), char>::value, "Error");
static_assert(is_same<__underlying_type(E3), int>::value, "Error");
static_assert(is_same<__underlying_type(E4), unsigned char>::value, "Error");
static_assert(is_same<__underlying_type(E5), int>::value, "Error");
static_assert(is_same<__underlying_type(E6), long>::value, "Error");
