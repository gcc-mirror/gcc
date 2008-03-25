// DR 339
//
// Test of the use of various assignment operators with SFINAE

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
    (sizeof(JOIN(check_,Name)(type<T&>(), type<U>())) == sizeof(yes_type)); \
}

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct Y { 
  Y& operator=(Y&);
};

struct X { 
  X& operator=(Y);
  X& operator+=(X);
  X& operator-=(X);
  X& operator*=(X);
  X& operator/=(X);
  X& operator%=(X);
  X& operator^=(X);
  X& operator&=(X);
  X& operator|=(X);
  X& operator<<=(X);
  X& operator>>=(X);
};
struct Z { };

// is_assignable
DEFINE_INFIX_BINARY_TRAIT(is_assignable, =);
STATIC_ASSERT((is_assignable<int>::value));
STATIC_ASSERT((is_assignable<int, long>::value));
STATIC_ASSERT((is_assignable<X>::value));
STATIC_ASSERT((!is_assignable<int*, int>::value));
STATIC_ASSERT((is_assignable<int*>::value));
STATIC_ASSERT((is_assignable<X, Y>::value));
STATIC_ASSERT((!is_assignable<X, Z>::value));
STATIC_ASSERT((!is_assignable<Y>::value));
STATIC_ASSERT((!is_assignable<const int, long>::value));

// has_plus_assign
DEFINE_INFIX_BINARY_TRAIT(has_plus_assign, +=);
X& operator+=(X&, Y);
STATIC_ASSERT((has_plus_assign<int>::value));
STATIC_ASSERT((has_plus_assign<int, long>::value));
STATIC_ASSERT((has_plus_assign<X>::value));
STATIC_ASSERT((has_plus_assign<int*, int>::value));
STATIC_ASSERT((!has_plus_assign<int*>::value));
STATIC_ASSERT((has_plus_assign<X, Y>::value));
STATIC_ASSERT((!has_plus_assign<X, Z>::value));
STATIC_ASSERT((!has_plus_assign<Y>::value));
STATIC_ASSERT((!has_plus_assign<const int, long>::value));

// has_minus_assign
DEFINE_INFIX_BINARY_TRAIT(has_minus_assign, -=);
X& operator-=(X&, Y);
STATIC_ASSERT((has_minus_assign<int>::value));
STATIC_ASSERT((has_minus_assign<int, long>::value));
STATIC_ASSERT((has_minus_assign<X>::value));
STATIC_ASSERT((has_minus_assign<int*, int>::value));
STATIC_ASSERT((!has_minus_assign<int*>::value));
STATIC_ASSERT((has_minus_assign<X, Y>::value));
STATIC_ASSERT((!has_minus_assign<X, Z>::value));
STATIC_ASSERT((!has_minus_assign<Y>::value));
STATIC_ASSERT((!has_minus_assign<int X::*>::value));
STATIC_ASSERT((!has_minus_assign<const int, long>::value));

// has_multiply_assign
DEFINE_INFIX_BINARY_TRAIT(has_multiply_assign, *=);
X& operator*=(X&, Y);
STATIC_ASSERT((has_multiply_assign<int>::value));
STATIC_ASSERT((has_multiply_assign<int, long>::value));
STATIC_ASSERT((has_multiply_assign<X>::value));
STATIC_ASSERT((!has_multiply_assign<int*, int>::value));
STATIC_ASSERT((!has_multiply_assign<int*>::value));
STATIC_ASSERT((has_multiply_assign<X, Y>::value));
STATIC_ASSERT((!has_multiply_assign<X, Z>::value));
STATIC_ASSERT((!has_multiply_assign<Y>::value));
STATIC_ASSERT((!has_multiply_assign<int X::*>::value));
STATIC_ASSERT((!has_multiply_assign<const int, long>::value));

// has_divide_assign
DEFINE_INFIX_BINARY_TRAIT(has_divide_assign, /=);
X& operator/=(X&, Y);
STATIC_ASSERT((has_divide_assign<int>::value));
STATIC_ASSERT((has_divide_assign<int, long>::value));
STATIC_ASSERT((has_divide_assign<X>::value));
STATIC_ASSERT((!has_divide_assign<int*, int>::value));
STATIC_ASSERT((!has_divide_assign<int*>::value));
STATIC_ASSERT((has_divide_assign<X, Y>::value));
STATIC_ASSERT((!has_divide_assign<X, Z>::value));
STATIC_ASSERT((!has_divide_assign<Y>::value));
STATIC_ASSERT((!has_divide_assign<int X::*>::value));

// has_remainder_assign
DEFINE_INFIX_BINARY_TRAIT(has_remainder_assign, %=);
X& operator%=(X&, Y);
STATIC_ASSERT((has_remainder_assign<int>::value));
STATIC_ASSERT((has_remainder_assign<int, long>::value));
STATIC_ASSERT((!has_remainder_assign<float>::value));
STATIC_ASSERT((has_remainder_assign<X>::value));
STATIC_ASSERT((!has_remainder_assign<int*, int>::value));
STATIC_ASSERT((!has_remainder_assign<int*>::value));
STATIC_ASSERT((has_remainder_assign<X, Y>::value));
STATIC_ASSERT((!has_remainder_assign<X, Z>::value));
STATIC_ASSERT((!has_remainder_assign<Y>::value));
STATIC_ASSERT((!has_remainder_assign<int X::*>::value));

// has_xor_assign
DEFINE_INFIX_BINARY_TRAIT(has_xor_assign, ^=);
X& operator^=(X&, Y);
STATIC_ASSERT((has_xor_assign<int>::value));
STATIC_ASSERT((has_xor_assign<int, long>::value));
STATIC_ASSERT((!has_xor_assign<float>::value));
STATIC_ASSERT((has_xor_assign<X>::value));
STATIC_ASSERT((!has_xor_assign<int*, int>::value));
STATIC_ASSERT((!has_xor_assign<int*>::value));
STATIC_ASSERT((has_xor_assign<X, Y>::value));
STATIC_ASSERT((!has_xor_assign<X, Z>::value));
STATIC_ASSERT((!has_xor_assign<Y>::value));
STATIC_ASSERT((!has_xor_assign<int X::*>::value));

// has_bitand_assign
DEFINE_INFIX_BINARY_TRAIT(has_bitand_assign, &=);
X& operator&=(X&, Y);
STATIC_ASSERT((has_bitand_assign<int>::value));
STATIC_ASSERT((has_bitand_assign<int, long>::value));
STATIC_ASSERT((!has_bitand_assign<float>::value));
STATIC_ASSERT((has_bitand_assign<X>::value));
STATIC_ASSERT((!has_bitand_assign<int*, int>::value));
STATIC_ASSERT((!has_bitand_assign<int*>::value));
STATIC_ASSERT((has_bitand_assign<X, Y>::value));
STATIC_ASSERT((!has_bitand_assign<X, Z>::value));
STATIC_ASSERT((!has_bitand_assign<Y>::value));
STATIC_ASSERT((!has_bitand_assign<int X::*>::value));

// has_bitor_assign
DEFINE_INFIX_BINARY_TRAIT(has_bitor_assign, |=);
X& operator|=(X&, Y);
STATIC_ASSERT((has_bitor_assign<int>::value));
STATIC_ASSERT((has_bitor_assign<int, long>::value));
STATIC_ASSERT((!has_bitor_assign<float>::value));
STATIC_ASSERT((has_bitor_assign<X>::value));
STATIC_ASSERT((!has_bitor_assign<int*, int>::value));
STATIC_ASSERT((!has_bitor_assign<int*>::value));
STATIC_ASSERT((has_bitor_assign<X, Y>::value));
STATIC_ASSERT((!has_bitor_assign<X, Z>::value));
STATIC_ASSERT((!has_bitor_assign<Y>::value));
STATIC_ASSERT((!has_bitor_assign<int X::*>::value));

// has_left_shift_assign
DEFINE_INFIX_BINARY_TRAIT(has_left_shift_assign, <<=);
X& operator<<=(X&, Y);
STATIC_ASSERT((has_left_shift_assign<int>::value));
STATIC_ASSERT((has_left_shift_assign<int, long>::value));
STATIC_ASSERT((!has_left_shift_assign<float>::value));
STATIC_ASSERT((has_left_shift_assign<X>::value));
STATIC_ASSERT((!has_left_shift_assign<int*, int>::value));
STATIC_ASSERT((!has_left_shift_assign<int*>::value));
STATIC_ASSERT((has_left_shift_assign<X, Y>::value));
STATIC_ASSERT((!has_left_shift_assign<X, Z>::value));
STATIC_ASSERT((!has_left_shift_assign<Y>::value));
STATIC_ASSERT((!has_left_shift_assign<int X::*>::value));

// has_right_shift_assign
DEFINE_INFIX_BINARY_TRAIT(has_right_shift_assign, >>=);
X& operator>>=(X&, Y);
STATIC_ASSERT((has_right_shift_assign<int>::value));
STATIC_ASSERT((has_right_shift_assign<int, long>::value));
STATIC_ASSERT((!has_right_shift_assign<float>::value));
STATIC_ASSERT((has_right_shift_assign<X>::value));
STATIC_ASSERT((!has_right_shift_assign<int*, int>::value));
STATIC_ASSERT((!has_right_shift_assign<int*>::value));
STATIC_ASSERT((has_right_shift_assign<X, Y>::value));
STATIC_ASSERT((!has_right_shift_assign<X, Z>::value));
STATIC_ASSERT((!has_right_shift_assign<Y>::value));
STATIC_ASSERT((!has_right_shift_assign<int X::*>::value));
