// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::bases_of.

#include <meta>

using namespace std::meta;

struct A {};
struct B : private virtual A {};
struct C : protected virtual A {};
struct D : public virtual B, protected virtual C {};
struct E : private virtual D {};
struct F : public virtual D, public virtual E {};
struct G : protected virtual E, private virtual F {};
struct H : protected virtual A {};
struct I : public virtual F, private virtual H {};
struct J : virtual G, virtual I {};

constexpr access_context gctx = access_context::current ();
constexpr access_context uctx = access_context::unchecked ();

static_assert (bases_of (^^A, uctx).size () == 0);
static_assert (bases_of (^^B, uctx).size () == 1);
static_assert (parent_of (bases_of (^^B, uctx)[0]) == ^^B);
static_assert (type_of (bases_of (^^B, uctx)[0]) == ^^A);
static_assert (offset_of (bases_of (^^B, uctx)[0]).total_bits () == 0);
static_assert (!is_accessible (bases_of (^^B, uctx)[0], gctx));
static_assert (bases_of (^^C, uctx).size () == 1);
static_assert (parent_of (bases_of (^^C, uctx)[0]) == ^^C);
static_assert (type_of (bases_of (^^C, uctx)[0]) == ^^A);
static_assert (offset_of (bases_of (^^C, uctx)[0]).total_bits () == 0);
static_assert (!is_accessible (bases_of (^^C, uctx)[0], gctx));
static_assert (bases_of (^^D, uctx).size () == 2);
static_assert (parent_of (bases_of (^^D, uctx)[0]) == ^^D);
static_assert (type_of (bases_of (^^D, uctx)[0]) == ^^B);
static_assert (offset_of (bases_of (^^D, uctx)[0]).total_bits () == 0);
static_assert (is_accessible (bases_of (^^D, uctx)[0], gctx));
static_assert (parent_of (bases_of (^^D, uctx)[1]) == ^^D);
static_assert (type_of (bases_of (^^D, uctx)[1]) == ^^C);
static_assert (offset_of (bases_of (^^D, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (void *));
static_assert (!is_accessible (bases_of (^^D, uctx)[1], gctx));
static_assert (bases_of (^^E, uctx).size () == 1);
static_assert (parent_of (bases_of (^^E, uctx)[0]) == ^^E);
static_assert (type_of (bases_of (^^E, uctx)[0]) == ^^D);
static_assert (offset_of (bases_of (^^E, uctx)[0]).total_bits () == 0);
static_assert (!is_accessible (bases_of (^^E, uctx)[0], gctx));
static_assert (bases_of (^^F, uctx).size () == 2);
static_assert (parent_of (bases_of (^^F, uctx)[0]) == ^^F);
static_assert (type_of (bases_of (^^F, uctx)[0]) == ^^D);
static_assert (offset_of (bases_of (^^F, uctx)[0]).total_bits () == __CHAR_BIT__ * sizeof (void *));
static_assert (is_accessible (bases_of (^^F, uctx)[0], gctx));
static_assert (parent_of (bases_of (^^F, uctx)[1]) == ^^F);
static_assert (type_of (bases_of (^^F, uctx)[1]) == ^^E);
static_assert (offset_of (bases_of (^^F, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (void *));
static_assert (is_accessible (bases_of (^^F, uctx)[1], gctx));
static_assert (bases_of (^^G, uctx).size () == 2);
static_assert (parent_of (bases_of (^^G, uctx)[0]) == ^^G);
static_assert (type_of (bases_of (^^G, uctx)[0]) == ^^E);
static_assert (offset_of (bases_of (^^G, uctx)[0]).total_bits () == 0);
static_assert (!is_accessible (bases_of (^^G, uctx)[0], gctx));
static_assert (parent_of (bases_of (^^G, uctx)[1]) == ^^G);
static_assert (type_of (bases_of (^^G, uctx)[1]) == ^^F);
static_assert (offset_of (bases_of (^^G, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (void *));
static_assert (!is_accessible (bases_of (^^G, uctx)[1], gctx));
static_assert (bases_of (^^H, uctx).size () == 1);
static_assert (parent_of (bases_of (^^H, uctx)[0]) == ^^H);
static_assert (type_of (bases_of (^^H, uctx)[0]) == ^^A);
static_assert (offset_of (bases_of (^^H, uctx)[0]).total_bits () == 0);
static_assert (!is_accessible (bases_of (^^H, uctx)[0], gctx));
static_assert (bases_of (^^I, uctx).size () == 2);
static_assert (parent_of (bases_of (^^I, uctx)[0]) == ^^I);
static_assert (type_of (bases_of (^^I, uctx)[0]) == ^^F);
static_assert (offset_of (bases_of (^^I, uctx)[0]).total_bits () == 0);
static_assert (is_accessible (bases_of (^^I, uctx)[0], gctx));
static_assert (parent_of (bases_of (^^I, uctx)[1]) == ^^I);
static_assert (type_of (bases_of (^^I, uctx)[1]) == ^^H);
static_assert (offset_of (bases_of (^^I, uctx)[1]).total_bits () == __CHAR_BIT__ * 2 * sizeof (void *));
static_assert (!is_accessible (bases_of (^^I, uctx)[1], gctx));
static_assert (bases_of (^^J, uctx).size () == 2);
static_assert (parent_of (bases_of (^^J, uctx)[0]) == ^^J);
static_assert (type_of (bases_of (^^J, uctx)[0]) == ^^G);
static_assert (offset_of (bases_of (^^J, uctx)[0]).total_bits () == 0);
static_assert (is_accessible (bases_of (^^J, uctx)[0], gctx));
static_assert (parent_of (bases_of (^^J, uctx)[1]) == ^^J);
static_assert (type_of (bases_of (^^J, uctx)[1]) == ^^I);
static_assert (offset_of (bases_of (^^J, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (void *));
static_assert (is_accessible (bases_of (^^J, uctx)[1], gctx));
