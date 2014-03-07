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

typedef __underlying_type(E1) UTE1;
typedef __underlying_type(E2) UTE2;
typedef __underlying_type(E3) UTE3;
typedef __underlying_type(E4) UTE4;
typedef __underlying_type(E5) UTE5;
typedef __underlying_type(E6) UTE6;

template<typename T>
  struct underlying_type
  { typedef __underlying_type(T) type; };

static_assert(is_same<underlying_type<E1>::type, UTE1>::value, "Error");
static_assert(is_same<underlying_type<E2>::type, UTE2>::value, "Error");
static_assert(is_same<underlying_type<E3>::type, UTE3>::value, "Error");
static_assert(is_same<underlying_type<E4>::type, UTE4>::value, "Error");
static_assert(is_same<underlying_type<E5>::type, UTE5>::value, "Error");
static_assert(is_same<underlying_type<E6>::type, UTE6>::value, "Error");

static_assert(is_same<underlying_type<E1>::type, unsigned>::value, "Error");
static_assert(is_same<underlying_type<E2>::type, char>::value, "Error");
static_assert(is_same<underlying_type<E3>::type, int>::value, "Error");
static_assert(is_same<underlying_type<E4>::type, 
	              unsigned char>::value, "Error");
static_assert(is_same<underlying_type<E5>::type, int>::value, "Error");
static_assert(is_same<underlying_type<E6>::type, long>::value, "Error");
