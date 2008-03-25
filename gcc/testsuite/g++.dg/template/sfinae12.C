// DR 339
//
// Test of the use of the ternary operator with SFINAE

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

template<typename T, typename U, typename V>
typename enable_if<
           (sizeof((create_a<T>()? create_a<U>() : create_a<V>()), 0) > 0),
           yes_type>::type
  check_ternary(int);
                                                            
template<typename T, typename U, typename V> no_type check_ternary(...);
                                                            
template<typename T, typename U, typename V>
struct has_ternary
{
  static const bool value =
    (sizeof(check_ternary<T, U, V>(0)) == sizeof(yes_type));
};

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct X { };
struct Y { operator bool(); };

STATIC_ASSERT((has_ternary<int, float, double>::value));
STATIC_ASSERT((has_ternary<bool, double, double>::value));
STATIC_ASSERT((!has_ternary<int, float*, double>::value));
STATIC_ASSERT((!has_ternary<X, double, double>::value));
STATIC_ASSERT((has_ternary<Y, double, double>::value));
