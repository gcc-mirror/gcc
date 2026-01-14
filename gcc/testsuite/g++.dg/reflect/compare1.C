// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test comparison of reflections.  Valid uses.

#include <meta>

namespace N {
  namespace M {
  }
}

namespace NN = N;
namespace NM = N::M;

static_assert(^^int == ^^int);
static_assert(^^int != ^^const int);
static_assert(^^int != ^^int &);
static_assert(^^int const == ^^const int);

static_assert(^^char == ^^char);
static_assert(^^char != ^^unsigned char);
static_assert(^^char != ^^signed char);

static_assert(^^:: == ^^::);
static_assert(^^:: != ^^N);
static_assert(^^N == ^^N);
static_assert(^^N != ^^N::M);
static_assert(^^N != ^^NN);
static_assert(^^N::M == ^^N::M);
static_assert(^^N::M != ^^NM);

using Alias = int;
static_assert(^^int != ^^Alias);
static_assert(^^int == std::meta::dealias (^^Alias));

namespace AliasNS = ::std;
static_assert(^^::std != ^^AliasNS);
static_assert(^^:: == parent_of(^^::std));
