// PR c++/123810
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

namespace A {
  typedef struct { int b; } B;
}
struct C {
  typedef struct { int d; } D;
};
struct F {
  typedef struct { int g; } G;
  typedef struct { int h; } H;
  typedef struct { int i; } I;
  typedef struct { int j; } J;
  typedef struct { int k; } K;
  typedef struct { int l; } L;
  typedef struct { int m; } M;
  typedef struct { int n; } N;
};
void
foo ()
{
  typedef struct { int e; } E;
  static_assert (is_type_alias (^^E));
}

static_assert (is_type_alias (^^A::B));
static_assert (is_type_alias (^^C::D));
static_assert (is_type_alias (^^F::G));
static_assert (is_type_alias (^^F::N));
constexpr auto uctx = std::meta::access_context::unchecked ();
static_assert (members_of (^^C, uctx)[0] == dealias (^^C::D));
static_assert (members_of (^^C, uctx)[1] == ^^C::D);
static_assert (members_of (^^F, uctx)[0] == dealias (^^F::G));
static_assert (members_of (^^F, uctx)[1] == ^^F::G);
static_assert (members_of (^^F, uctx)[2] == dealias (^^F::H));
static_assert (members_of (^^F, uctx)[3] == ^^F::H);
static_assert (members_of (^^F, uctx)[4] == dealias (^^F::I));
static_assert (members_of (^^F, uctx)[5] == ^^F::I);
static_assert (members_of (^^F, uctx)[6] == dealias (^^F::J));
static_assert (members_of (^^F, uctx)[7] == ^^F::J);
static_assert (members_of (^^F, uctx)[8] == dealias (^^F::K));
static_assert (members_of (^^F, uctx)[9] == ^^F::K);
static_assert (members_of (^^F, uctx)[10] == dealias (^^F::L));
static_assert (members_of (^^F, uctx)[11] == ^^F::L);
static_assert (members_of (^^F, uctx)[12] == dealias (^^F::M));
static_assert (members_of (^^F, uctx)[13] == ^^F::M);
static_assert (members_of (^^F, uctx)[14] == dealias (^^F::N));
static_assert (members_of (^^F, uctx)[15] == ^^F::N);
static_assert ((members_of (^^A, uctx)[0] == dealias (^^A::B)
		&& members_of (^^A, uctx)[1] == ^^A::B)
	       || ((members_of (^^A, uctx)[0] == ^^A::B)
		   && members_of (^^A, uctx)[1] == dealias (^^A::B)));
