// { dg-do compile }
// { dg-options "-Wunused -O3" }

static const int i = 0;
static void f() { }		/* { dg-warning "defined but not used" "" { xfail *-*-* } } */
static inline void g() { }
