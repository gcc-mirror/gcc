/* Test constraints on unary '&': PR 22367.  */

/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

extern void v;
void f1 (void) { &v; } /* { dg-error "taking address of expression of type 'void'" } */

extern void *pv;
void f2 (void) { &*pv; } /* { dg-warning "dereferencing" } */

extern const void cv;
void f3 (void) { &cv; }

extern const void *pcv;
void f4 (void) { &*pcv; } /* { dg-warning "dereferencing" } */
