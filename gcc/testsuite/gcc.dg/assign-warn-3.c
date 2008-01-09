/* Test diagnostics for bad type conversion when inlining unprototyped
   functions: should not be errors with -pedantic-errors.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-O3 -std=c99 -pedantic-errors" } */

/* This is valid to execute, so maybe shouldn't warn at all.  */
static inline void f0(x) signed char *x; { }
void g0(unsigned char *x) { f0(x); } /* { dg-warning "warning: pointer targets in passing argument 1 of 'f0' differ in signedness" } */

/* This is undefined on execution but still must compile.  */
static inline void f1(x) int *x; { }
void g1(unsigned int *x) { f1(x); } /* { dg-warning "warning: pointer targets in passing argument 1 of 'f1' differ in signedness" } */
