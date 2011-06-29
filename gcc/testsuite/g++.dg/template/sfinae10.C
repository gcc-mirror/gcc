// DR 339
//
// Test of the use of various unary operators with SFINAE

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

#define DEFINE_PREFIX_UNARY_TRAIT(Name,Op)                      \
template<typename T>                                            \
  typename enable_if<(sizeof(Op create_a<T>(), 1) > 0),         \
		     yes_type>::type                            \
  JOIN(check_,Name)(int);                                       \
                                                                \
template<typename T>                                            \
  no_type JOIN(check_,Name)(...);                               \
                                                                \
template<typename T>                                            \
struct Name                                                     \
{                                                               \
  static const bool value =                                     \
    (sizeof(JOIN(check_,Name)<T&>(0)) == sizeof(yes_type));     \
}

#define DEFINE_POSTFIX_UNARY_TRAIT(Name,Op)                     \
template<typename T>                                            \
  typename enable_if<(sizeof(create_a<T>() Op, 1) > 0),         \
		     yes_type>::type                            \
  JOIN(check_,Name)(int);                                       \
                                                                \
template<typename T>                                            \
  no_type JOIN(check_,Name)(...);                               \
                                                                \
template<typename T>                                            \
struct Name                                                     \
{                                                               \
  static const bool value =                                     \
    (sizeof(JOIN(check_,Name)<T&>(0)) == sizeof(yes_type));     \
}

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

struct W {
  W operator+();
  W operator-();
  int operator*();
  W operator~();
  bool operator!();
  W& operator++();
  W& operator--();
  W& operator++(int);
  W& operator--(int);
};

struct X { };
X operator+(X);
X operator-(X);
int operator*(X);
X operator~(X);
bool operator!(X);
X& operator++(X&);
X& operator--(X&);
X& operator++(X&, int);
X& operator--(X&, int);

struct Y { };

struct Z {
private:
  Z operator+(); // { dg-error "is private" }
  Z operator-(); // { dg-error "is private" }
  int operator*(); // { dg-error "is private" }
  Z operator~(); // { dg-error "is private" } 
  bool operator!(); // { dg-error "is private" }  
  Z& operator++(); // { dg-error "is private" }  
  Z& operator--(); // { dg-error "is private" }  
  Z& operator++(int); // { dg-error "is private" }  
  Z& operator--(int); // { dg-error "is private" }  
};

// has_unary_plus
DEFINE_PREFIX_UNARY_TRAIT(has_unary_plus, +); // { dg-error "within this context" }
STATIC_ASSERT((has_unary_plus<int>::value));
STATIC_ASSERT((!has_unary_plus<int X::*>::value));
STATIC_ASSERT((has_unary_plus<W>::value));
STATIC_ASSERT((has_unary_plus<X>::value));
STATIC_ASSERT((!has_unary_plus<Y>::value));

// is_negatable
DEFINE_PREFIX_UNARY_TRAIT(is_negatable, -); // { dg-error "within this context" }
STATIC_ASSERT((is_negatable<int>::value));
STATIC_ASSERT((!is_negatable<int X::*>::value));
STATIC_ASSERT((is_negatable<W>::value));
STATIC_ASSERT((is_negatable<X>::value));
STATIC_ASSERT((!is_negatable<Y>::value));

// is_dereferenceable
DEFINE_PREFIX_UNARY_TRAIT(is_dereferenceable, *); // { dg-error "within this context" }
STATIC_ASSERT((!is_dereferenceable<int>::value));
STATIC_ASSERT((is_dereferenceable<int*>::value));
STATIC_ASSERT((is_dereferenceable<W>::value));
STATIC_ASSERT((is_dereferenceable<X>::value));
STATIC_ASSERT((!is_dereferenceable<Y>::value));

// has_bitwise_not
DEFINE_PREFIX_UNARY_TRAIT(has_bitwise_not, ~); // { dg-error "within this context" }
STATIC_ASSERT((has_bitwise_not<int>::value));
STATIC_ASSERT((!has_bitwise_not<int*>::value));
STATIC_ASSERT((has_bitwise_not<W>::value));
STATIC_ASSERT((has_bitwise_not<X>::value));
STATIC_ASSERT((!has_bitwise_not<Y>::value));

// has_truth_not
DEFINE_PREFIX_UNARY_TRAIT(has_truth_not, !); // { dg-error "within this context" }
STATIC_ASSERT((has_truth_not<int>::value));
STATIC_ASSERT((has_truth_not<int*>::value));
STATIC_ASSERT((has_truth_not<W>::value));
STATIC_ASSERT((has_truth_not<X>::value));
STATIC_ASSERT((!has_truth_not<Y>::value));

// has_preincrement
DEFINE_PREFIX_UNARY_TRAIT(has_preincrement, ++); // { dg-error "within this context" }
STATIC_ASSERT((has_preincrement<int>::value));
STATIC_ASSERT((has_preincrement<int*>::value));
STATIC_ASSERT((!has_preincrement<int X::*>::value));
STATIC_ASSERT((has_preincrement<W>::value));
STATIC_ASSERT((has_preincrement<X>::value));
STATIC_ASSERT((!has_preincrement<Y>::value));

// has_predecrement
DEFINE_PREFIX_UNARY_TRAIT(has_predecrement, --); // { dg-error "within this context" }
STATIC_ASSERT((has_predecrement<int>::value));
STATIC_ASSERT((has_predecrement<int*>::value));
STATIC_ASSERT((!has_predecrement<int X::*>::value));
STATIC_ASSERT((has_predecrement<W>::value));
STATIC_ASSERT((has_predecrement<X>::value));
STATIC_ASSERT((!has_predecrement<Y>::value));

// has_postincrement
DEFINE_POSTFIX_UNARY_TRAIT(has_postincrement, ++); // { dg-error "within this context" }
STATIC_ASSERT((has_postincrement<int>::value));
STATIC_ASSERT((has_postincrement<int*>::value));
STATIC_ASSERT((!has_postincrement<int X::*>::value));
STATIC_ASSERT((has_postincrement<W>::value));
STATIC_ASSERT((has_postincrement<X>::value));
STATIC_ASSERT((!has_postincrement<Y>::value));

// has_postdecrement
DEFINE_POSTFIX_UNARY_TRAIT(has_postdecrement, --); // { dg-error "within this context" }
STATIC_ASSERT((has_postdecrement<int>::value));
STATIC_ASSERT((has_postdecrement<int*>::value));
STATIC_ASSERT((!has_postdecrement<int X::*>::value));
STATIC_ASSERT((has_postdecrement<W>::value));
STATIC_ASSERT((has_postdecrement<X>::value));
STATIC_ASSERT((!has_postdecrement<Y>::value));

// Check for private members
STATIC_ASSERT((has_unary_plus<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((is_negatable<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((is_dereferenceable<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((has_bitwise_not<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((has_truth_not<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((has_preincrement<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((has_predecrement<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((has_postincrement<Z>::value)); // { dg-message "required from here" }
STATIC_ASSERT((has_postdecrement<Z>::value)); // { dg-message "required from here" }

