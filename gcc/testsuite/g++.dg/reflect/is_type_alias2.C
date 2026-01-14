// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_type_alias.

#include <meta>

using namespace std::meta;

template<typename T>
struct S { };

using U = S<int>;

template<typename T>
using V = S<T*>;

typedef int T;

static_assert (!is_type_alias (^^S<int>));
static_assert (is_type_alias (^^U));
static_assert (!is_type_alias (^^V));
static_assert (is_type_alias (^^V<int>));
static_assert (is_type_alias (^^T));
static_assert (!is_type_alias (^^wchar_t));
static_assert (is_type_alias (^^size_t));

using A = void(int, int);
static_assert (is_type_alias (^^A));
static_assert (!is_type_alias (dealias(^^A)));
