/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

struct S1 { char x; char : 0; char y; };
struct S2 { char x; int : 0; char y; };
#if __BITINT_MAXWIDTH__ >= 575
struct S3 { char x; _BitInt(575) : 0; char y; };
#endif
#if __BITINT_MAXWIDTH__ >= 389
struct S4 { char x; _BitInt(195) a : 63; _BitInt(282) b : 280; _BitInt(389) c : 23; _BitInt(2) d : 1; char y; };
#endif
#if __BITINT_MAXWIDTH__ >= 192
struct S5 { char x; _BitInt(192) a : 191; unsigned _BitInt(192) b : 190; _BitInt(192) c : 189; char y; };
#endif
struct S6 { _BitInt(2) a : 1; };
#if __BITINT_MAXWIDTH__ >= 389
struct S4 s4;
static_assert (expr_has_type (s4.a + 1uwb, _BitInt(195)));
static_assert (expr_has_type (s4.b + 1uwb, _BitInt(282)));
static_assert (expr_has_type (s4.c + 1uwb, _BitInt(389)));
static_assert (expr_has_type (s4.d * 0wb, _BitInt(2)));
#endif
#if __BITINT_MAXWIDTH__ >= 192
struct S5 s5;
static_assert (expr_has_type (s5.a + 1uwb, _BitInt(192)));
static_assert (expr_has_type (s5.b + 1wb, unsigned _BitInt(192)));
static_assert (expr_has_type (s5.c + 1uwb, _BitInt(192)));
#endif
struct S6 s6;
static_assert (expr_has_type (s6.a + 0wb, _BitInt(2)));
#if defined(__x86_64__) && __LP64__ && __BITINT_MAXWIDTH__ >= 575
static_assert (sizeof (struct S1) == 2);
static_assert (sizeof (struct S2) == 5);
static_assert (sizeof (struct S3) == 9);
static_assert (sizeof (struct S4) == 48);
static_assert (sizeof (struct S5) == 88);
static_assert (sizeof (struct S6) == 1);
static_assert (alignof (struct S1) == 1);
static_assert (alignof (struct S2) == 1);
static_assert (alignof (struct S3) == 1);
static_assert (alignof (struct S4) == 8);
static_assert (alignof (struct S5) == 8);
static_assert (alignof (struct S6) == 1);
#endif
