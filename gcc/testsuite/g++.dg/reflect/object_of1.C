// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::object_of.

#include <meta>

using namespace std::meta;

int x;
int& y = x;

static_assert (^^x != ^^y);
static_assert (object_of (^^x) == object_of (^^y));
static_assert (is_object (object_of (^^x)));
static_assert (is_object (object_of (^^y)));

constexpr int i = 1;
static_assert (object_of (^^i) == reflect_object (i));
static_assert (is_object (object_of (^^i)));

constexpr const int *p = &i;
constexpr auto ro = object_of (^^p);
static_assert (^^p != ro);
static_assert (!is_variable (ro));
static_assert (is_object (ro));
static_assert (!is_value (ro));
static_assert (ro == reflect_object (p));
static_assert (ro != reflect_constant (&i));

void
g ()
{
  constexpr int &r = x;
  constexpr auto r1 = object_of (^^r);
  static_assert (is_object (object_of (^^r)));
  static int sl = 42;
  constexpr auto r2 = object_of (^^sl);
  static_assert (is_object (object_of (^^sl)));
}

const int &rc = 42;
// Here, object_of yields _ZGR2rc_.
static_assert (object_of (^^rc) != reflect_constant (42));
static_assert (!is_variable (object_of (^^rc)));
static_assert (is_object (object_of (^^rc)));
static_assert (!is_value (object_of (^^rc)));

struct A { const int ci = 0; int nci; };
struct B : A { mutable int i; };

B arr[2];
static_assert (object_of (^^arr) != ^^arr);
static_assert (is_object (object_of (^^arr)));
static_assert (reflect_object (arr) == object_of (^^arr));
static_assert (reflect_object (arr[0]) != object_of (^^arr));
static_assert (type_of (object_of (^^arr)) == ^^B[2]);

const A &r = arr[1];
static_assert (object_of (^^r) != ^^r);
static_assert (is_object (object_of (^^r)));
static_assert (reflect_object (arr[1]) != object_of (^^r));
// TODO Fails because the array index has types long int x int.
//static_assert (reflect_object (static_cast<A&>(arr[1])) == object_of (^^r));
static_assert (type_of (^^r) == ^^const A&);
static_assert (type_of (object_of (^^r)) == ^^A);

struct S { int i; } s;
static_assert (object_of (^^s) != ^^s);
static_assert (is_object (object_of (^^s)));
static_assert (reflect_object (s) == object_of (^^s));
static_assert (type_of (object_of (^^s)) == ^^S);

template<int, int&, S>
struct X { };
constexpr auto T = ^^X<1, x, S{}>;
constexpr auto o1 = object_of (template_arguments_of (T)[1]);
constexpr auto o2 = object_of (template_arguments_of (T)[2]);
static_assert (is_object (o1));
static_assert (is_object (o2));
static_assert (o1 == template_arguments_of (T)[1]);
static_assert (o2 == template_arguments_of (T)[2]);
static_assert (o1 == reflect_object (x));
static_assert (type_of (o1) == ^^int);
// ??? May not be true.
static_assert (type_of (o2) == ^^const S);
