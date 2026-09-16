/* PR c/127383 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

struct S { _BitInt(195) a : 193; const unsigned _BitInt(195) b : 193; volatile _BitInt(195) c : 193; const volatile unsigned _BitInt(195) d : 193; };
struct S s;
struct S foo ();
static_assert (expr_has_type (+s.a, _BitInt(195)));
static_assert (expr_has_type (+s.b, unsigned _BitInt(195)));
static_assert (expr_has_type (+s.c, _BitInt(195)));
static_assert (expr_has_type (+s.d, unsigned _BitInt(195)));
static_assert (expr_has_type ((_BitInt(193)) s.a, _BitInt(193)));
static_assert (expr_has_type ((unsigned _BitInt(193)) s.b, unsigned _BitInt(193)));
static_assert (expr_has_type ((_BitInt(193)) s.c, _BitInt(193)));
static_assert (expr_has_type ((unsigned _BitInt(193)) s.d, unsigned _BitInt(193)));
static_assert (expr_has_type (+(_BitInt(193)) s.a, _BitInt(193)));
static_assert (expr_has_type (+(unsigned _BitInt(193)) s.b, unsigned _BitInt(193)));
static_assert (expr_has_type (+(_BitInt(193)) s.c, _BitInt(193)));
static_assert (expr_has_type (+(unsigned _BitInt(193)) s.d, unsigned _BitInt(193)));
static_assert (expr_has_type ((const _BitInt(193)) s.a, _BitInt(193)));
static_assert (expr_has_type ((volatile unsigned _BitInt(193)) s.b, unsigned _BitInt(193)));
static_assert (expr_has_type ((const volatile _BitInt(193)) s.c, _BitInt(193)));
static_assert (expr_has_type ((const unsigned _BitInt(193)) s.d, unsigned _BitInt(193)));
static_assert (expr_has_type (+(_BitInt(193)) s.a, _BitInt(193)));
static_assert (expr_has_type (+(unsigned _BitInt(193)) s.b, unsigned _BitInt(193)));
static_assert (expr_has_type (+(_BitInt(193)) s.c, _BitInt(193)));
static_assert (expr_has_type (+(unsigned _BitInt(193)) s.d, unsigned _BitInt(193)));
static_assert (expr_has_type (+foo ().a, _BitInt(195)));
static_assert (expr_has_type (+foo ().b, unsigned _BitInt(195)));
static_assert (expr_has_type (+foo ().c, _BitInt(195)));
static_assert (expr_has_type (+foo ().d, unsigned _BitInt(195)));
static_assert (expr_has_type ((_BitInt(193)) foo ().a, _BitInt(193)));
static_assert (expr_has_type ((unsigned _BitInt(193)) foo ().b, unsigned _BitInt(193)));
static_assert (expr_has_type ((_BitInt(193)) foo ().c, _BitInt(193)));
static_assert (expr_has_type ((unsigned _BitInt(193)) foo ().d, unsigned _BitInt(193)));
static_assert (expr_has_type (+(_BitInt(193)) foo ().a, _BitInt(193)));
static_assert (expr_has_type (+(unsigned _BitInt(193)) foo ().b, unsigned _BitInt(193)));
static_assert (expr_has_type (+(_BitInt(193)) foo ().c, _BitInt(193)));
static_assert (expr_has_type (+(unsigned _BitInt(193)) foo ().d, unsigned _BitInt(193)));
static_assert (expr_has_type ((const _BitInt(193)) foo ().a, _BitInt(193)));
static_assert (expr_has_type ((volatile unsigned _BitInt(193)) foo ().b, unsigned _BitInt(193)));
static_assert (expr_has_type ((const volatile _BitInt(193)) foo ().c, _BitInt(193)));
static_assert (expr_has_type ((const unsigned _BitInt(193)) foo ().d, unsigned _BitInt(193)));
