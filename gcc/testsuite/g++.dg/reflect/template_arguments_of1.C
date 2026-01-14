// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::template_arguments_of.

#include <meta>

using namespace std::meta;

template<class T, class U = T> struct Pair { };
template<class T> struct Pair<char, T> { };
template<class T> using PairPtr = Pair<T*>;

static_assert(template_of(^^Pair<int>) == ^^Pair);
static_assert(template_of(^^Pair<char, char>) == ^^Pair);
static_assert(template_arguments_of(^^Pair<int>).size() == 2);
static_assert(template_arguments_of(^^Pair<int>)[0] == ^^int);
static_assert(template_arguments_of(^^Pair<int>)[1] == ^^int);

static_assert(template_of(^^PairPtr<int>) == ^^PairPtr);
static_assert(template_arguments_of(^^PairPtr<int>).size() == 1);
static_assert(template_arguments_of(^^PairPtr<int>)[0] == ^^int);

struct S { };
int i;
template<int, int&, S, template<class> class>
  struct X { };
constexpr auto T = ^^X<1, i, S{}, PairPtr>;
static_assert(is_value(template_arguments_of(T)[0]));
static_assert(!is_object(template_arguments_of(T)[0]));
static_assert(!is_value(template_arguments_of(T)[1]));
static_assert(is_object(template_arguments_of(T)[1]));
static_assert(is_object(template_arguments_of(T)[2]));
static_assert(!is_value(template_arguments_of(T)[2]));
static_assert(!is_value(template_arguments_of(T)[3]));
static_assert(!is_object(template_arguments_of(T)[3]));
static_assert(template_arguments_of(T)[3] == ^^PairPtr);
