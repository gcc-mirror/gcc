// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::variant_{size,alternative}.

#include <meta>
#include <variant>

using namespace std::meta;

using V0 = std::variant<>;
static_assert (variant_size (^^V0) == 0);
static_assert (variant_size (^^const V0) == 0);

using V1 = std::variant<void *>;
static_assert (variant_size (^^V1) == 1);
static_assert (variant_size (^^const V1) == 1);
static_assert (variant_alternative (0, ^^V1) == ^^void *);

using V4 = std::variant<long, long, void *, double>;
static_assert (variant_size (^^V4) == 4);
static_assert (variant_size (^^const V4) == 4);
static_assert (variant_alternative (0, ^^V4) == ^^long);
static_assert (variant_alternative (1, ^^V4) == ^^long);
static_assert (variant_alternative (2, ^^V4) == ^^void *);
static_assert (variant_alternative (3, ^^V4) == ^^double);
// cv-qualification on the variant type propagates to the extracted alternative type.
static_assert (variant_alternative (0, ^^const V4) == ^^const long);
static_assert (variant_alternative (1, ^^const V4) == ^^const long);
static_assert (variant_alternative (2, ^^const V4) == ^^void *const);
static_assert (variant_alternative (3, ^^const V4) == ^^const double);

using mytype1 = float;
using mytype2 = mytype1 *;
using V2 = std::variant<mytype1, mytype2>;
static_assert (variant_size (^^V2) == 2);
static_assert (variant_alternative (0, ^^V2) == ^^float);
static_assert (variant_alternative (1, ^^V2) == ^^float *);
