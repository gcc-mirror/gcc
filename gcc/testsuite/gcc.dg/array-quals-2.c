/* Test that pointers to arrays of differently qualified types aren't
   permitted in conditional expressions, and that qualifiers aren't
   lost in forming composite types.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */
typedef const char T[1];
typedef const char T2[1];
typedef volatile char U[1];
T *p;
T2 *p2;
U *q;
void *f(void) { return 1 ? p : q; } /* { dg-warning "pointer type mismatch in conditional expression" } */
T *g(void) { return 1 ? p : p2; }
