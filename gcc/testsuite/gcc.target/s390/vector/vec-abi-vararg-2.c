/* Check calling convention in the vector ABI.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -Wno-implicit-function-declaration" } */


typedef long v2di __attribute__((vector_size(16)));
extern v2di foo1 (int, v2di);
extern v2di foo2 (int, int);
extern v2di foo3 (int, ...);

v2di bar1 (int a)  { return foo2 (1, a); }
v2di bar2 (int a)  { return foo3 (1, a); }
v2di bar3 (v2di a) { return foo1 (1, a); }
v2di bar4 (v2di a) { return foo3 (1, a); }

int bar5 (int a)  { return foo4 (1, a); }
int bar6 (v2di a) { return foo4 (1, a); } /* { dg-error "vector argument passed to unprototyped function" } */
