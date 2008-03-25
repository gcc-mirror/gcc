// DR 339
//
// Test of the use of the comma operator with SFINAE

// Boilerplate helpers
typedef char yes_type;
struct no_type { char data[2]; };

template<typename T> T create_a();
template<typename T> struct type { };

template<bool, typename T = void> struct enable_if { typedef T type; };
template<typename T> struct enable_if<false, T> { };

#define JOIN( X, Y ) DO_JOIN( X, Y )
#define DO_JOIN( X, Y ) DO_JOIN2(X,Y)
#define DO_JOIN2( X, Y ) X##Y

template<typename T, typename U>
  typename enable_if<(sizeof(create_a<T>(), create_a<U>()) > 0),
		     yes_type>::type
  check_comma(int);
                                                            
template<typename T, typename U> no_type check_comma(...);
                                                            
template<typename T, typename U>
struct has_comma
{
  static const bool value =
    (sizeof(check_comma<T, U>(0)) == sizeof(yes_type));
};

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct X { };
struct Y { };
struct Z { };

bool operator,(X&, Y);
bool operator,(X, Z);
void operator,(const Y&, const Z&);

STATIC_ASSERT((has_comma<int, float>::value));
STATIC_ASSERT((has_comma<int, X>::value));
STATIC_ASSERT((has_comma<X, X>::value));
STATIC_ASSERT((has_comma<X, Y>::value));
STATIC_ASSERT((has_comma<X&, Y>::value));
STATIC_ASSERT((has_comma<X, Z>::value));
STATIC_ASSERT((!has_comma<Y, Z>::value));
