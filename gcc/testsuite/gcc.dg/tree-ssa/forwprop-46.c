/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi" } */

/* A single insert into a one-element vector overwrites the whole vector,
   it used to ICE in simple_dce_from_worklist when the vector was a
   default definition.  */

typedef long long v1di __attribute__((vector_size(8)));
typedef double v1df __attribute__((vector_size(8)));
typedef long long v2di __attribute__((vector_size(16)));

v1di f1 (v1di a, v1di b) { a[0] = b[0]; return a; }
v1df f2 (v1df a, v1df b) { a[0] = b[0]; return a; }
v1di f3 (v1di a, v2di b) { a[0] = b[1]; return a; }
v1di f4 (v1di a, long long x) { a[0] = x; return a; }
