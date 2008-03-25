// DR 339
//
// Test of the use of casts with SFINAE

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

#define CHECK_CAST(CastKind)                                            \
template<typename T, typename U>                                        \
 typename enable_if<(sizeof((JOIN(CastKind,_cast)<U>(create_a<T>())), 0) > 0), \
                   yes_type>::type                                      \
  JOIN(check_,JOIN(CastKind,_cast))(int);                               \
                                                                        \
template<typename T, typename U>                                        \
  no_type JOIN(check_,JOIN(CastKind,_cast))(...);                       \
                                                                        \
template<typename T, typename U>                                        \
struct JOIN(has_,JOIN(CastKind,_cast))                                  \
{                                                                       \
  static const bool value =                                             \
    (sizeof(JOIN(check_,JOIN(CastKind,_cast))<T, U>(0)) == sizeof(yes_type)); \
}

template<typename T, typename U>
typename enable_if<(sizeof(((U)create_a<T>()), 0) > 0), yes_type>::type
  check_c_cast(int);
                                                            
template<typename T, typename U> no_type check_c_cast(...);
                                                            
template<typename T, typename U>
struct has_c_cast
{
  static const bool value =
    (sizeof(check_c_cast<T, U>(0)) == sizeof(yes_type));
};

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

CHECK_CAST(static);
CHECK_CAST(dynamic);
CHECK_CAST(const);
CHECK_CAST(reinterpret);

struct X { virtual void f(); };
struct Y { operator bool(); };
struct Z : public X { };

STATIC_ASSERT((has_static_cast<int, float>::value));
STATIC_ASSERT((!has_static_cast<X, Y>::value));
STATIC_ASSERT((has_static_cast<Z, X>::value));

STATIC_ASSERT(!(has_dynamic_cast<int, float>::value));
STATIC_ASSERT(!(has_dynamic_cast<X, Y>::value));
STATIC_ASSERT(!(has_dynamic_cast<X, Z>::value));
STATIC_ASSERT(!(has_dynamic_cast<Y, Z>::value));
STATIC_ASSERT((has_dynamic_cast<X*, Z*>::value));
STATIC_ASSERT((has_dynamic_cast<X*, Y*>::value));
STATIC_ASSERT(!(has_dynamic_cast<Y*, Z*>::value));

STATIC_ASSERT(!(has_const_cast<int, float>::value));
STATIC_ASSERT((has_const_cast<const int*, int*>::value));
STATIC_ASSERT((has_const_cast<int*, const int*>::value));
STATIC_ASSERT(!(has_const_cast<const int*, float*>::value));

STATIC_ASSERT((has_reinterpret_cast<int*, float*>::value));
STATIC_ASSERT(!(has_reinterpret_cast<void*, char>::value));
STATIC_ASSERT(!(has_reinterpret_cast<const X, X>::value));

STATIC_ASSERT((has_c_cast<int, float>::value));
STATIC_ASSERT(!(has_c_cast<X, Y>::value));
STATIC_ASSERT(!(has_c_cast<void*, char>::value));
