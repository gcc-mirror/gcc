/* { dg-do compile } */
/* { dg-options "-Wunused -O3" } */

static const int i = 0;
static void f() { }		/* { dg-warning "defined but not used" } */
static inline void g() { }
