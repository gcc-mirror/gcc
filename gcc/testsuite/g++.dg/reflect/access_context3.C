// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::access_context.

#include <meta>

using namespace std::meta;

struct Ctor
{
  consteval Ctor()
  { }

  access_context ctx = access_context::current ();
};

constexpr Ctor ctor;
static_assert (is_constructor (ctor.ctx.scope ()) && parent_of(ctor.ctx.scope()) == ^^Ctor);

struct Dfltd
{
  Dfltd() = default;

  access_context ctx = access_context::current ();
};

constexpr Dfltd dfltd;
static_assert (is_constructor (dfltd.ctx.scope ()) && parent_of(dfltd.ctx.scope()) == ^^Dfltd);

struct Aggr {
  access_context ctx = access_context::current ();
};

constexpr Aggr aggrCtor;
static_assert (is_constructor (aggrCtor.ctx.scope ()) && parent_of(aggrCtor.ctx.scope()) == ^^Aggr);

constexpr Aggr aggrInit{};
static_assert (aggrInit.ctx.scope () == ^^::);

consteval Aggr
func(Aggr aggr = {})
{ return aggr; }

static_assert (func().ctx.scope () == ^^::);

constexpr void
bar()
{
  static_assert (func().ctx.scope () == ^^bar);
}
