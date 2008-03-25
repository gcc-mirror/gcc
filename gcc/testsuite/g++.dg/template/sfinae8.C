// DR 339
//
// Test of the use of various boolean binary operators with SFINAE

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

bool accepts_bool(bool);

#define DEFINE_BINARY_PREDICATE_TRAIT(Name,Op)				\
template<typename T, typename U>					\
  typename enable_if<sizeof(accepts_bool(create_a<T>() Op create_a<U>())), \
		     yes_type>::type					\
  JOIN(check_,Name)(type<T>, type<U>);		                        \
									\
no_type JOIN(check_,Name)(...);						\
									\
template<typename T, typename U = T>					\
struct Name								\
{									\
  static const bool value =						\
    (sizeof(JOIN(check_,Name)(type<T>(), type<U>())) == sizeof(yes_type)); \
}

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct X { };
struct Y { };

struct convertible_to_bool { 
  operator int convertible_to_bool::* (); 
};

struct not_convertible_to_bool { };

// is_less_than_comparable
DEFINE_BINARY_PREDICATE_TRAIT(is_less_than_comparable,<);
bool                    operator<(X, X);
convertible_to_bool     operator<(X, Y);
not_convertible_to_bool operator<(Y, X);

STATIC_ASSERT((is_less_than_comparable<int>::value));
STATIC_ASSERT((is_less_than_comparable<int, long>::value));
STATIC_ASSERT((is_less_than_comparable<int*>::value));
STATIC_ASSERT((is_less_than_comparable<X>::value));
STATIC_ASSERT((is_less_than_comparable<X, Y>::value));
STATIC_ASSERT((!is_less_than_comparable<Y, X>::value));
STATIC_ASSERT((!is_less_than_comparable<Y>::value));

// is_less_equal_comparable
DEFINE_BINARY_PREDICATE_TRAIT(is_less_equal_comparable,<=);
bool                    operator<=(X, X);
convertible_to_bool     operator<=(X, Y);
not_convertible_to_bool operator<=(Y, X);

STATIC_ASSERT((is_less_equal_comparable<int>::value));
STATIC_ASSERT((is_less_equal_comparable<int, long>::value));
STATIC_ASSERT((is_less_equal_comparable<int*>::value));
STATIC_ASSERT((is_less_equal_comparable<X>::value));
STATIC_ASSERT((is_less_equal_comparable<X, Y>::value));
STATIC_ASSERT((!is_less_equal_comparable<Y, X>::value));
STATIC_ASSERT((!is_less_equal_comparable<Y>::value));

// is_greater_than_comparable
DEFINE_BINARY_PREDICATE_TRAIT(is_greater_than_comparable,>);
bool                    operator>(X, X);
convertible_to_bool     operator>(X, Y);
not_convertible_to_bool operator>(Y, X);

STATIC_ASSERT((is_greater_than_comparable<int>::value));
STATIC_ASSERT((is_greater_than_comparable<int, long>::value));
STATIC_ASSERT((is_greater_than_comparable<int*>::value));
STATIC_ASSERT((is_greater_than_comparable<X>::value));
STATIC_ASSERT((is_greater_than_comparable<X, Y>::value));
STATIC_ASSERT((!is_greater_than_comparable<Y, X>::value));
STATIC_ASSERT((!is_greater_than_comparable<Y>::value));

// is_greater_equal_comparable
DEFINE_BINARY_PREDICATE_TRAIT(is_greater_equal_comparable,>=);
bool                    operator>=(X, X);
convertible_to_bool     operator>=(X, Y);
not_convertible_to_bool operator>=(Y, X);

STATIC_ASSERT((is_greater_equal_comparable<int>::value));
STATIC_ASSERT((is_greater_equal_comparable<int, long>::value));
STATIC_ASSERT((is_greater_equal_comparable<int*>::value));
STATIC_ASSERT((is_greater_equal_comparable<X>::value));
STATIC_ASSERT((is_greater_equal_comparable<X, Y>::value));
STATIC_ASSERT((!is_greater_equal_comparable<Y, X>::value));
STATIC_ASSERT((!is_greater_equal_comparable<Y>::value));

// is_equality_comparable
struct Z : X { };
DEFINE_BINARY_PREDICATE_TRAIT(is_equality_comparable,==);
bool                    operator==(X, X);
convertible_to_bool     operator==(X, Y);
not_convertible_to_bool operator==(Y, X);

STATIC_ASSERT((is_equality_comparable<int>::value));
STATIC_ASSERT((is_equality_comparable<int, long>::value));
STATIC_ASSERT((is_equality_comparable<int*>::value));
STATIC_ASSERT((is_equality_comparable<X>::value));
STATIC_ASSERT((is_equality_comparable<X, Y>::value));
STATIC_ASSERT((!is_equality_comparable<Y, X>::value));
STATIC_ASSERT((!is_equality_comparable<Y>::value));
STATIC_ASSERT((is_equality_comparable<int X::*>::value));
STATIC_ASSERT((!is_equality_comparable<int X::*, int Y::*>::value));
STATIC_ASSERT((is_equality_comparable<int*, float*>::value));
STATIC_ASSERT((is_equality_comparable<X*, Z*>::value));
STATIC_ASSERT((!is_equality_comparable<X*, Y*>::value));

// is_not_equal_comparable
DEFINE_BINARY_PREDICATE_TRAIT(is_not_equal_comparable,!=);
bool                    operator!=(X, X);
convertible_to_bool     operator!=(X, Y);
not_convertible_to_bool operator!=(Y, X);

STATIC_ASSERT((is_not_equal_comparable<int>::value));
STATIC_ASSERT((is_not_equal_comparable<int, long>::value));
STATIC_ASSERT((is_not_equal_comparable<int*>::value));
STATIC_ASSERT((is_not_equal_comparable<X>::value));
STATIC_ASSERT((is_not_equal_comparable<X, Y>::value));
STATIC_ASSERT((!is_not_equal_comparable<Y, X>::value));
STATIC_ASSERT((!is_not_equal_comparable<Y>::value));
STATIC_ASSERT((is_not_equal_comparable<int X::*>::value));
STATIC_ASSERT((!is_not_equal_comparable<int X::*, int Y::*>::value));
STATIC_ASSERT((is_not_equal_comparable<int*, float*>::value));
STATIC_ASSERT((is_not_equal_comparable<X*, Z*>::value));
STATIC_ASSERT((!is_not_equal_comparable<X*, Y*>::value));

// has_logical_and
DEFINE_BINARY_PREDICATE_TRAIT(has_logical_and,&&);
bool                    operator&&(X, X);
convertible_to_bool     operator&&(X, Y);
not_convertible_to_bool operator&&(Y, X);

STATIC_ASSERT((has_logical_and<int>::value));
STATIC_ASSERT((has_logical_and<int, long>::value));
STATIC_ASSERT((has_logical_and<int*>::value));
STATIC_ASSERT((has_logical_and<X>::value));
STATIC_ASSERT((has_logical_and<X, Y>::value));
STATIC_ASSERT((!has_logical_and<Y, X>::value));
STATIC_ASSERT((!has_logical_and<Y>::value));
STATIC_ASSERT((has_logical_and<int X::*>::value));
STATIC_ASSERT((has_logical_and<int X::*, int Y::*>::value));
STATIC_ASSERT((has_logical_and<int*, float*>::value));
STATIC_ASSERT((has_logical_and<X*, Z*>::value));
STATIC_ASSERT((has_logical_and<X*, Y*>::value));

// has_logical_or
DEFINE_BINARY_PREDICATE_TRAIT(has_logical_or,||);
bool                    operator||(X, X);
convertible_to_bool     operator||(X, Y);
not_convertible_to_bool operator||(Y, X);

STATIC_ASSERT((has_logical_or<int>::value));
STATIC_ASSERT((has_logical_or<int, long>::value));
STATIC_ASSERT((has_logical_or<int*>::value));
STATIC_ASSERT((has_logical_or<X>::value));
STATIC_ASSERT((has_logical_or<X, Y>::value));
STATIC_ASSERT((!has_logical_or<Y, X>::value));
STATIC_ASSERT((!has_logical_or<Y>::value));
STATIC_ASSERT((has_logical_or<int X::*>::value));
STATIC_ASSERT((has_logical_or<int X::*, int Y::*>::value));
STATIC_ASSERT((has_logical_or<int*, float*>::value));
STATIC_ASSERT((has_logical_or<X*, Z*>::value));
STATIC_ASSERT((has_logical_or<X*, Y*>::value));
