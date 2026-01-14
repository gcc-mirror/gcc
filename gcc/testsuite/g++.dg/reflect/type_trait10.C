// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], other
// transformations.

#include <meta>
using namespace std::meta;

class ClassType { };
using CT = ClassType;

static_assert (remove_cvref (^^const volatile int) == ^^int);
static_assert (remove_cvref (^^const volatile int *) == ^^const volatile int *);
static_assert (remove_cvref (^^const volatile int &) == ^^int);
static_assert (remove_cvref (^^const volatile int &&) == ^^int);
static_assert (remove_cvref (^^const volatile ClassType) == ^^ClassType);
static_assert (remove_cvref (^^const volatile ClassType *) == ^^const volatile ClassType *);
static_assert (remove_cvref (^^const volatile ClassType &) == ^^ClassType);
static_assert (remove_cvref (^^const volatile ClassType &&) == ^^ClassType);
static_assert (remove_cvref (^^CT) == ^^ClassType);
static_assert (remove_cvref (^^CT &) == ^^ClassType);
static_assert (remove_cvref (^^const CT &) == ^^ClassType);
static_assert (remove_cvref (^^const int (&) [3]) == ^^int [3]);
static_assert (remove_cvref (^^const int (&) ()) == ^^const int ());

static_assert (decay (^^bool) == ^^bool);
static_assert (decay (^^int) == ^^int);
static_assert (decay (^^ClassType) == ^^ClassType);
static_assert (decay (^^CT) == ^^ClassType);
static_assert (decay (^^const int) == ^^int);
static_assert (decay (^^volatile const long) == ^^long);
static_assert (decay (^^int &) == ^^int);
static_assert (decay (^^int &&) == ^^int);
static_assert (decay (^^CT &) == ^^ClassType);
static_assert (decay (^^volatile const int &) == ^^int);
static_assert (decay (^^const int &&) == ^^int);
static_assert (decay (^^int [4]) == ^^int *);
static_assert (decay (^^const int []) == ^^const int *);
static_assert (decay (^^int [5][2][1]) == ^^int (*)[2][1]);
static_assert (decay (^^const int [][5][1]) == ^^const int (*)[5][1]);
static_assert (decay (^^int (int)) == ^^int (*) (int));

enum E1 : unsigned { };
enum E2 : char { };
enum class E3 { };
enum class E4 : unsigned char { c = 1 };
enum class E5 : int { a = -1, b = 1 };
enum class E6 : long { c = __LONG_MAX__ };
enum E7 { E70 = 0 };
enum E8 { E80 = ~0UL };

static_assert (underlying_type (^^E1) == ^^unsigned);
static_assert (underlying_type (^^E2) == ^^char);
static_assert (underlying_type (^^E3) == ^^int);
static_assert (underlying_type (^^E4) == ^^unsigned char);
static_assert (underlying_type (^^E5) == ^^int);
static_assert (underlying_type (^^E6) == ^^long);
static_assert (is_integral_type (underlying_type (^^E7)));
static_assert (is_integral_type (underlying_type (^^E8)));

struct S;
struct T;
template <typename T>
struct U
{
};
typedef int int2;
struct V {};
namespace
{
  struct W {};
}

template <typename T, typename U>
struct eq
{
  constexpr eq ()
  {
    static_assert (type_order (^^T, ^^U) == std::strong_ordering::equal);
    static_assert (type_order (^^U, ^^T) == std::strong_ordering::equal);
  }
};
template <typename T, typename U>
struct ne
{
  constexpr ne ()
  {
    static_assert (type_order (^^T, ^^U) != std::strong_ordering::equal);
    static_assert (type_order (^^U, ^^T) != std::strong_ordering::equal);
    static_assert (type_order (^^T, ^^U) == std::strong_ordering::greater
                   ? type_order (^^U, ^^T) == std::strong_ordering::less
                   : type_order (^^U, ^^T) == std::strong_ordering::greater);
  }
};

constexpr eq <void, void> a;
constexpr eq <const void, const void> b;
constexpr eq <int, int> c;
constexpr eq <long int, long int> d;
constexpr eq <const volatile unsigned, const volatile unsigned> e;
constexpr eq <S, S> f;
constexpr eq <U <int>, U <int>> g;
constexpr eq <unsigned[2], unsigned[2]> h;
constexpr eq <int, int2> i;
constexpr eq <int (*) (int, long), int (*) (int, long)> j;
constexpr ne <int, long> k;
constexpr ne <const int, int> l;
constexpr ne <S, T> m;
constexpr ne <int &, int &&> n;
constexpr ne <U <S>, U <T>> o;
constexpr ne <U <short>, U <char>> p;
constexpr ne <int (*) (int, long), int (*) (int, int)> q;
constexpr eq <W, W> r;
constexpr ne <V, W> s;
constexpr eq <U <W>, U <W>> t;
constexpr ne <U <V>, U <W>> u;
