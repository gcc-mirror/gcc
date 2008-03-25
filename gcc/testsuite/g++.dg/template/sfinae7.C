// DR 339
//
// Test of the use of various binary operators with SFINAE

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

#define DEFINE_INFIX_BINARY_TRAIT(Name,Op)				\
template<typename T, typename U>					\
  typename enable_if<(sizeof(create_a<T>() Op create_a<U>(), 1) > 0),	\
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

template<typename T, typename U>
  typename enable_if<(sizeof(create_a<T>()[create_a<U>()], 1) > 0),
                     yes_type>::type
  check_subscript(int);

template<typename T, typename U>
  no_type check_subscript(...);

template<typename T, typename U>
struct can_subscript
{
  static const bool value = 
    (sizeof(check_subscript<T, U>(0)) == sizeof(yes_type));
};

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct X { };
struct Y { int operator[](X); };

// is_addable
DEFINE_INFIX_BINARY_TRAIT(is_addable, +);
X operator+(X, X);
X operator+(X, Y);
STATIC_ASSERT((is_addable<int>::value));
STATIC_ASSERT((is_addable<int, long>::value));
STATIC_ASSERT((is_addable<X>::value));
STATIC_ASSERT((is_addable<int*, int>::value));
STATIC_ASSERT((!is_addable<int*>::value));
STATIC_ASSERT((is_addable<X, Y>::value));
STATIC_ASSERT((!is_addable<Y>::value));

// is_subtractable
DEFINE_INFIX_BINARY_TRAIT(is_subtractable, -);
X operator-(X, X);
X operator-(X, Y);
STATIC_ASSERT((is_subtractable<int>::value));
STATIC_ASSERT((is_subtractable<int, long>::value));
STATIC_ASSERT((is_subtractable<X>::value));
STATIC_ASSERT((is_subtractable<int*, int>::value));
STATIC_ASSERT((is_subtractable<int*>::value));
STATIC_ASSERT((is_subtractable<X, Y>::value));
STATIC_ASSERT((!is_subtractable<Y>::value));
STATIC_ASSERT((!is_subtractable<int X::*>::value));

// is_multiplicable
DEFINE_INFIX_BINARY_TRAIT(is_multiplicable, *);
X operator*(X, X);
X operator*(X, Y);
STATIC_ASSERT((is_multiplicable<int>::value));
STATIC_ASSERT((is_multiplicable<int, long>::value));
STATIC_ASSERT((is_multiplicable<X>::value));
STATIC_ASSERT((!is_multiplicable<int*, int>::value));
STATIC_ASSERT((!is_multiplicable<int*>::value));
STATIC_ASSERT((is_multiplicable<X, Y>::value));
STATIC_ASSERT((!is_multiplicable<Y>::value));
STATIC_ASSERT((!is_multiplicable<int X::*>::value));

// is_divisible
DEFINE_INFIX_BINARY_TRAIT(is_divisible, /);
X operator/(X, X);
X operator/(X, Y);
STATIC_ASSERT((is_divisible<int>::value));
STATIC_ASSERT((is_divisible<int, long>::value));
STATIC_ASSERT((is_divisible<X>::value));
STATIC_ASSERT((!is_divisible<int*, int>::value));
STATIC_ASSERT((!is_divisible<int*>::value));
STATIC_ASSERT((is_divisible<X, Y>::value));
STATIC_ASSERT((!is_divisible<Y>::value));
STATIC_ASSERT((!is_divisible<int X::*>::value));

// has_remainder
DEFINE_INFIX_BINARY_TRAIT(has_remainder, %);
X operator%(X, X);
X operator%(X, Y);
STATIC_ASSERT((has_remainder<int>::value));
STATIC_ASSERT((has_remainder<int, long>::value));
STATIC_ASSERT((!has_remainder<float>::value));
STATIC_ASSERT((has_remainder<X>::value));
STATIC_ASSERT((!has_remainder<int*, int>::value));
STATIC_ASSERT((!has_remainder<int*>::value));
STATIC_ASSERT((has_remainder<X, Y>::value));
STATIC_ASSERT((!has_remainder<Y>::value));
STATIC_ASSERT((!has_remainder<int X::*>::value));

// has_xor
DEFINE_INFIX_BINARY_TRAIT(has_xor, ^);
X operator^(X, X);
X operator^(X, Y);
STATIC_ASSERT((has_xor<int>::value));
STATIC_ASSERT((has_xor<int, long>::value));
STATIC_ASSERT((!has_xor<float>::value));
STATIC_ASSERT((has_xor<X>::value));
STATIC_ASSERT((!has_xor<int*, int>::value));
STATIC_ASSERT((!has_xor<int*>::value));
STATIC_ASSERT((has_xor<X, Y>::value));
STATIC_ASSERT((!has_xor<Y>::value));
STATIC_ASSERT((!has_xor<int X::*>::value));

// has_bitand
DEFINE_INFIX_BINARY_TRAIT(has_bitand, &);
X operator&(X, X);
X operator&(X, Y);
STATIC_ASSERT((has_bitand<int>::value));
STATIC_ASSERT((has_bitand<int, long>::value));
STATIC_ASSERT((!has_bitand<float>::value));
STATIC_ASSERT((has_bitand<X>::value));
STATIC_ASSERT((!has_bitand<int*, int>::value));
STATIC_ASSERT((!has_bitand<int*>::value));
STATIC_ASSERT((has_bitand<X, Y>::value));
STATIC_ASSERT((!has_bitand<Y>::value));
STATIC_ASSERT((!has_bitand<int X::*>::value));

// has_bitor
DEFINE_INFIX_BINARY_TRAIT(has_bitor, |);
X operator|(X, X);
X operator|(X, Y);
STATIC_ASSERT((has_bitor<int>::value));
STATIC_ASSERT((has_bitor<int, long>::value));
STATIC_ASSERT((!has_bitor<float>::value));
STATIC_ASSERT((has_bitor<X>::value));
STATIC_ASSERT((!has_bitor<int*, int>::value));
STATIC_ASSERT((!has_bitor<int*>::value));
STATIC_ASSERT((has_bitor<X, Y>::value));
STATIC_ASSERT((!has_bitor<Y>::value));
STATIC_ASSERT((!has_bitor<int X::*>::value));

// has_left_shift
DEFINE_INFIX_BINARY_TRAIT(has_left_shift, <<);
X operator<<(X, X);
X operator<<(X, Y);
STATIC_ASSERT((has_left_shift<int>::value));
STATIC_ASSERT((has_left_shift<int, long>::value));
STATIC_ASSERT((!has_left_shift<float>::value));
STATIC_ASSERT((has_left_shift<X>::value));
STATIC_ASSERT((!has_left_shift<int*, int>::value));
STATIC_ASSERT((!has_left_shift<int*>::value));
STATIC_ASSERT((has_left_shift<X, Y>::value));
STATIC_ASSERT((!has_left_shift<Y>::value));
STATIC_ASSERT((!has_left_shift<int X::*>::value));

// has_right_shift
DEFINE_INFIX_BINARY_TRAIT(has_right_shift, >>);
X operator>>(X, X);
X operator>>(X, Y);
STATIC_ASSERT((has_right_shift<int>::value));
STATIC_ASSERT((has_right_shift<int, long>::value));
STATIC_ASSERT((!has_right_shift<float>::value));
STATIC_ASSERT((has_right_shift<X>::value));
STATIC_ASSERT((!has_right_shift<int*, int>::value));
STATIC_ASSERT((!has_right_shift<int*>::value));
STATIC_ASSERT((has_right_shift<X, Y>::value));
STATIC_ASSERT((!has_right_shift<Y>::value));
STATIC_ASSERT((!has_right_shift<int X::*>::value));

// can_subscript
STATIC_ASSERT((can_subscript<int*, int>::value));
STATIC_ASSERT((can_subscript<int, int*>::value));
STATIC_ASSERT((can_subscript<int(&)[7], int>::value));
STATIC_ASSERT((can_subscript<int, int(&)[7]>::value));
STATIC_ASSERT((!can_subscript<X, Y>::value));
STATIC_ASSERT((can_subscript<Y, X>::value));
