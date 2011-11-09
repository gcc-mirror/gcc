// DR 339
//
// Test of the use of the function call operator with SFINAE
typedef char yes_type;
struct no_type { char data[2]; };

template<typename T> T create_a();

template<typename T> struct type { };

template<bool, typename T = void> struct enable_if { typedef T type; };
template<typename T> struct enable_if<false, T> { };

template<typename F, typename T1, typename T2>
  typename enable_if<sizeof(create_a<F>()(create_a<T1>(), create_a<T2>()), 1),
		     yes_type>::type
  check_is_callable2(type<F>, type<T1>, type<T2>); // { dg-error "within this context" "" { target c++11 } }

no_type check_is_callable2(...);

template<typename F, typename T1, typename T2 = T1>
struct is_callable2
{
  static const bool value = 
    (sizeof(check_is_callable2(type<F>(), type<T1>(), type<T2>()))
     == sizeof(yes_type)); // { dg-error "within this context" }
};

#define JOIN( X, Y ) DO_JOIN( X, Y )
#define DO_JOIN( X, Y ) DO_JOIN2(X,Y)
#define DO_JOIN2( X, Y ) X##Y

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif


struct A;
struct B;

struct A {
  A(B);
};

struct B {
  B(A);
};

struct F {
  void operator()(A, A);

private:
  void operator()(B, B); // { dg-error "is private" }
};

STATIC_ASSERT((is_callable2<F, B, B>::value));
