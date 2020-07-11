/* { dg-require-effective-target arm_arch_v8a_hard_ok } */
/* { dg-options "-std=c++11 -O -foptimize-sibling-calls" } */
/* { dg-add-options arm_arch_v8a_hard } */
/* { dg-final { check-function-bodies "**" "" "" } } */

struct X { };
struct Y { int : 0; };
struct Z { int : 0; Y y; };
struct W : public X { X q; };

struct A { float a; };

struct B : public X { float a; };
struct C : public Y { float a; };
struct D : public Z { float a; };
struct E : public W { float a; };

struct F { [[no_unique_address]] X x; float a; };
struct G { [[no_unique_address]] Y y; float a; };
struct H { [[no_unique_address]] Z z; float a; };
struct I { [[no_unique_address]] W w; float a; };

struct J { float a; [[no_unique_address]] X x; float b; };
struct K { float a; [[no_unique_address]] Y y; float b; };
struct L { float a; [[no_unique_address]] Z z; float b; };
struct M { float a; [[no_unique_address]] W w; float b; };

struct N : public A { float b; };
struct O { [[no_unique_address]] A a; float b; };

struct P : public Y { int : 0; float a, b, c, d; };

union Q { X x; float a; };
union R { [[no_unique_address]] X x; float a; };

union S { A a; float b; };
union T { F f; float b; };
union U { N n; O o; };

typedef S Salias;
typedef T Talias;
typedef U Ualias;

#define T(S, s) extern int callee_##s (S)

/*
** _Z8caller_aR1A:
**	vldr.32	s0, \[r0\]
**	b	.*
*/
T (A, a); int caller_a (A &a) { return callee_a (a); } /* { dg-bogus {argument of type 'A'} } */

/*
** _Z8caller_bR1B:
**	vldr.32	s0, \[r0\]
**	b	.*
*/
T (B, b); int caller_b (B &b) { return callee_b (b); } /* { dg-bogus {argument of type 'B'} } */

/*
** _Z8caller_cR1C:
**	vldr.32	s0, \[r0\]
**	b	.*
*/
T (C, c); int caller_c (C &c) { return callee_c (c); } /* { dg-bogus {argument of type 'C'} } */

/*
** _Z8caller_dR1D:
**	ldm	r0, {r0, r1}
**	b	.*
*/
T (D, d); int caller_d (D &d) { return callee_d (d); } /* { dg-bogus {argument of type 'D'} } */

/*
** _Z8caller_eR1E:
**	ldm	r0, {r0, r1}
**	b	.*
*/
T (E, e); int caller_e (E &e) { return callee_e (e); } /* { dg-bogus {argument of type 'E'} } */

/*
** _Z8caller_fR1F:
**	vldr.32	s0, \[r0\]
**	b	.*
*/
T (F, f); int caller_f (F &f) { return callee_f (f); } /* { dg-message {parameter passing for argument of type 'F' with '\[\[no_unique_address\]\]' members changed in GCC 10.1} } */

/*
** _Z8caller_gR1G:
**	vldr.32	s0, \[r0\]
**	b	.*
*/
T (G, g); int caller_g (G &g) { return callee_g (g); } /* { dg-message {parameter passing for argument of type 'G' with '\[\[no_unique_address\]\]' members changed in GCC 10.1} } */

/*
** _Z8caller_hR1H:
**	ldm	r0, {r0, r1}
**	b	.*
*/
T (H, h); int caller_h (H &h) { return callee_h (h); } /* { dg-bogus {argument of type 'H'} } */

/*
** _Z8caller_iR1I:
**	ldm	r0, {r0, r1}
**	b	.*
*/
T (I, i); int caller_i (I &i) { return callee_i (i); } /* { dg-bogus {argument of type 'I'} } */

/*
** _Z8caller_jR1J:
**	vldr.32	s0, \[r0\]
**	vldr.32	s1, \[r0, #4\]
**	b	.*
*/
T (J, j); int caller_j (J &j) { return callee_j (j); } /* { dg-message {parameter passing for argument of type 'J' with '\[\[no_unique_address\]\]' members changed in GCC 10.1} } */

/*
** _Z8caller_kR1K:
**	vldr.32	s0, \[r0\]
**	vldr.32	s1, \[r0, #4\]
**	b	.*
*/
T (K, k); int caller_k (K &k) { return callee_k (k); } /* { dg-message {parameter passing for argument of type 'K' with '\[\[no_unique_address\]\]' members changed in GCC 10.1} } */

/*
** _Z8caller_lR1L:
**	ldm	r0, {r0, r1, r2}
**	b	.*
*/
T (L, l); int caller_l (L &l) { return callee_l (l); } /* { dg-bogus {argument of type 'L'} } */

/*
** _Z8caller_mR1M:
**	ldm	r0, {r0, r1, r2}
**	b	.*
*/
T (M, m); int caller_m (M &m) { return callee_m (m); } /* { dg-bogus {argument of type 'M'} } */

/*
** _Z8caller_nR1N:
**	vldr.32	s0, \[r0\]
**	vldr.32	s1, \[r0, #4\]
**	b	.*
*/
T (N, n); int caller_n (N &n) { return callee_n (n); } /* { dg-bogus {argument of type 'N'} } */

/*
** _Z8caller_oR1O:
**	vldr.32	s0, \[r0\]
**	vldr.32	s1, \[r0, #4\]
**	b	.*
*/
T (O, o); int caller_o (O &o) { return callee_o (o); } /* { dg-bogus {argument of type 'O'} } */

/*
** _Z8caller_pR1P:
**	vldr.32	s0, \[r0\]
**	vldr.32	s1, \[r0, #4\]
**	vldr.32	s2, \[r0, #8\]
**	vldr.32	s3, \[r0, #12\]
**	b	.*
*/
T (P, p); int caller_p (P &p) { return callee_p (p); } /* { dg-bogus {argument of type 'P'} } */

/*
** _Z8caller_qR1Q:
**	ldr	r0, \[r0\]
**	b	.*
*/
T (Q, q); int caller_q (Q &q) { return callee_q (q); } /* { dg-bogus {argument of type 'Q'} } */

/*
** _Z8caller_rR1R:
**	ldr	r0, \[r0\]
**	b	.*
*/
T (R, r); int caller_r (R &r) { return callee_r (r); } /* { dg-bogus {argument of type 'R'} } */

/*
** _Z8caller_sR1S:
**	vldr.32	s0, \[r0\]	@ int
**	b	.*
*/
T (Salias, s); int caller_s (Salias &s) { return callee_s (s); } /* { dg-bogus {argument of type 'S'} } */

/*
** _Z8caller_tR1T:
**	vldr.32	s0, \[r0\]	@ int
**	b	.*
*/
T (Talias, t); int caller_t (Talias &t) { return callee_t (t); } /* { dg-message {parameter passing for argument of type 'T' with '\[\[no_unique_address\]\]' members changed in GCC 10.1} } */

/*
** _Z8caller_uR1U:
**	vldr.32	s0, \[r0\]
**	vldr.32	s1, \[r0, #4\]
**	b	.*
*/
T (Ualias, u); int caller_u (Ualias &u) { return callee_u (u); } /* { dg-bogus {argument of type 'U'} } */

/* { dg-bogus {argument of type 'const} "should not be printed as const" { target *-*-*} 0 } */
