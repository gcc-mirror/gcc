// DR 339
//
// Test of the use of the new and new[] operators with SFINAE

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

template<typename T>
typename enable_if<(sizeof(new T, 0) > 0), yes_type>::type
  check_new(int);
                                                            
template<typename T> no_type check_new(...);
                                                            
template<typename T>
struct has_new
{
  static const bool value =
    (sizeof(check_new<T>(0)) == sizeof(yes_type));
};

template<typename T, typename U>
typename enable_if<(sizeof((new T(create_a<U>())), 0) > 0),
                   yes_type>::type
  check_new_one_arg(int);
                                                            
template<typename T, typename U> no_type check_new_one_arg(...);
                                                            
template<typename T, typename U>
struct has_new_one_arg
{
  static const bool value =
    (sizeof(check_new_one_arg<T, U>(0)) == sizeof(yes_type));
};

template<typename T, typename U, U N>
typename enable_if<(sizeof(new T[N], 0) > 0), yes_type>::type
  check_array_new(int);
                                                            
template<typename T, typename U, U N> no_type check_array_new(...);
                                                            
template<typename T, typename U, U N>
struct has_array_new
{
  static const bool value =
    (sizeof(check_array_new<T, U, N>(0)) == sizeof(yes_type));
};

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct X { 
  X(int);
};

struct Y { int foo; };

STATIC_ASSERT((has_new<Y>::value));
STATIC_ASSERT(!(has_new<X>::value));
STATIC_ASSERT((has_new_one_arg<Y, Y>::value));
STATIC_ASSERT((has_new_one_arg<X, float>::value));
STATIC_ASSERT(!(has_new_one_arg<X, int X::*>::value));

STATIC_ASSERT((has_array_new<Y, int, 5>::value));
STATIC_ASSERT(!(has_array_new<X, int Y::*, &Y::foo>::value));
STATIC_ASSERT(!(has_array_new<X, int, 5>::value));
