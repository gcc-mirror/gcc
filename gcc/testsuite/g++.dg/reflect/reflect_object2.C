// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_object.

#include <meta>

using namespace std::meta;

static int obj = 42;
constexpr auto robj = reflect_object (obj);
static const int cobj = 42;
constexpr auto rcobj = reflect_object (cobj);
static_assert ([:rcobj:] == 42);
static constexpr int ceobj = 42;
constexpr auto rceobj = reflect_object (ceobj);
static_assert ([:rceobj:] == 42);

static_assert (reflect_object (obj) != ^^obj);
static_assert (type_of (reflect_object (obj)) == ^^int);
static_assert (is_object (reflect_object (obj)));
static_assert (!is_value (reflect_object (obj)));
static_assert (!is_variable (reflect_object (obj)));
static_assert (reflect_object (cobj) != ^^cobj);
static_assert (type_of (reflect_object (cobj)) == ^^const int);
static_assert (is_object (reflect_object (cobj)));
static_assert (!is_value (reflect_object (cobj)));
static_assert (!is_variable (reflect_object (cobj)));
static_assert (reflect_object (ceobj) != ^^ceobj);
static_assert (type_of (reflect_object (ceobj)) == ^^const int);
static_assert (is_object (reflect_object (ceobj)));
static_assert (!is_value (reflect_object (ceobj)));
static_assert (!is_variable (reflect_object (ceobj)));

static_assert (!is_const (reflect_object (obj)));
static_assert (is_const (reflect_object (cobj)));
static_assert (is_const (reflect_object (ceobj)));

const int &ref = obj;
static_assert (reflect_object (ref) != ^^obj);
static_assert (type_of (reflect_object (ref)) == ^^int);
static_assert (is_object (reflect_object (ref)));
static_assert (!is_value (reflect_object (ref)));
static_assert (!is_variable (reflect_object (ref)));
static_assert (!is_const (reflect_object (ref)));

constexpr const int *pobj = &ceobj;
static_assert (reflect_object (pobj) != ^^pobj);
static_assert (type_of (reflect_object (pobj)) == ^^const int *const);
static_assert (is_object (reflect_object (pobj)));
static_assert (!is_value (reflect_object (pobj)));
static_assert (!is_variable (reflect_object (pobj)));
static_assert (is_const (reflect_object (pobj)));

static constexpr std::pair<int, short> p = {1, 2};
static_assert (reflect_object (p) != ^^p);
static_assert (reflect_object (p.first) != ^^p);
static_assert (type_of (reflect_object (p)) == ^^const std::pair<int, short>);
static_assert (&[:reflect_object (p.first):] == &p.first);
static_assert ([:reflect_object (p.first):] == 1);
static_assert (type_of (reflect_object (p.first)) == ^^const int);
static_assert (&[:reflect_object (p.second):] == &p.second);
static_assert ([:reflect_object (p.second):] == 2);
static_assert (type_of(reflect_object (p.second)) == ^^const short);
static_assert (is_object (reflect_object (p)));
static_assert (!is_value (reflect_object (p)));
static_assert (is_object (reflect_object (p.first)));
static_assert (!is_value (reflect_object (p.first)));

struct B {};
struct D : B {};
D d;
static_assert (type_of (reflect_object (d)) == ^^D);
// TODO
//static_assert (type_of (reflect_object (static_cast<B &>(d))) == ^^B);

template<typename T>
constexpr T
bar (T a)
{
  return a + 42;
}

constexpr int (*barp)(int) = &bar<int>;
constexpr auto rbar = reflect_object (barp);
static_assert ([:rbar:](0) == 42);

unsigned long letter;
static_assert (size_of (reflect_object (letter)) == sizeof (unsigned long));

template<auto K> constexpr auto R = reflect_object (K);
B b = [:R<B{}>:];

struct S { int m; };
constexpr S s{42};
static_assert (reflect_object (s) != ^^s);
static_assert (type_of (reflect_object (s)) == ^^const S);
static_assert (is_object (reflect_object (s)));
static_assert (!is_variable (reflect_object (s)));
static_assert (is_const (reflect_object (s)));

template<auto V>
  requires (is_class_type (^^decltype(V)))
consteval bool
fn ()
{
  return reflect_constant (V) == reflect_object (V);
}
static_assert (fn<s>());

void
g ()
{
  int i = [:robj:];
  [:rbar:](42);
}
