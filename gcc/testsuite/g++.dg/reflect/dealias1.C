// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::dealias.

#include <meta>

using namespace std::meta;

namespace N { }

using int_alias = int;
using int_alias_alias = int_alias;
template <typename T> using alias_templ = T;

typedef int T1;
typedef T1 T2;

namespace N_alias = N;
namespace N_alias_alias = N_alias;

typename [: dealias (^^int) :] i1 = 42;
typename [: dealias (^^int_alias_alias) :] i2 = 42;

static_assert (dealias (^^int) == ^^int);
static_assert (dealias (^^int_alias) == ^^int);
static_assert (dealias (^^int_alias_alias) == ^^int);
static_assert (dealias (^^alias_templ<int_alias_alias>) == ^^int);
static_assert (dealias (^^T1) == ^^int);
static_assert (dealias (^^T2) == ^^int);

static_assert (dealias (^^::) == ^^::);
static_assert (dealias (^^N) == ^^N);
static_assert (dealias (^^N_alias) == ^^N);
static_assert (dealias (^^N_alias_alias) == ^^N);

static_assert (dealias (std::meta::info{}) == std::meta::info{});
