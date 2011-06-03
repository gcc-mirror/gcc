// { dg-do compile }

struct A { int const i; };
struct B { int& i; };
struct C { int i; };

template< class T >
class is_constructible_via_new_without_initializer
{
    template<int> class size {};

    typedef char yes_type;
    struct no_type { char data[2]; };

    template <class U>
    static yes_type sfinae (size< sizeof (new U) >*);

    template <class U>
    static no_type sfinae (...);

public:
  static const bool value = sizeof (sfinae<T>(0)) == sizeof (yes_type);
};

#define JOIN( X, Y ) DO_JOIN( X, Y )
#define DO_JOIN( X, Y ) DO_JOIN2(X,Y)
#define DO_JOIN2( X, Y ) X##Y

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#  define STATIC_ASSERT(Expr) static_assert(Expr, #Expr)
#else
#  define STATIC_ASSERT(Expr) int JOIN(a,__LINE__)[Expr? 1 : -1]
#endif

STATIC_ASSERT (!is_constructible_via_new_without_initializer<A>::value);
STATIC_ASSERT (!is_constructible_via_new_without_initializer<B>::value);
STATIC_ASSERT (is_constructible_via_new_without_initializer<C>::value);

