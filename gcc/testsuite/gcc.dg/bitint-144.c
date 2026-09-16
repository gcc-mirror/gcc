/* PR c/127383 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c2y" } */

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)
enum E : _BitInt(195) { E0 = 0, E1 = 1, EM1 = -1 };
enum F : unsigned _BitInt(195) { F0 = 0, F1 = 1 };

struct S { enum E a : 193; const enum F b : 193; volatile enum E c : 193; const volatile enum F d : 193; };
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
