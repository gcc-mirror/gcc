/* Check calling convention in the vector ABI for single element vectors.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "vlr\t%v24,%v26" 7 } } */

typedef int  __attribute__((vector_size(16))) v4si;

typedef char __attribute__((vector_size(1))) v1qi;
typedef short int __attribute__((vector_size(2))) v1hi;
typedef int __attribute__((vector_size(4))) v1si;
typedef long long __attribute__((vector_size(8))) v1di;
typedef float __attribute__((vector_size(4))) v1sf;
typedef double __attribute__((vector_size(8))) v1df;
typedef long double __attribute__((vector_size(16))) v1tf;

v1qi foo1 (v4si a, v1qi b) { return b; }
v1hi foo2 (v4si a, v1hi b) { return b; }
v1si foo3 (v4si a, v1si b) { return b; }
v1di foo4 (v4si a, v1di b) { return b; }
v1sf foo5 (v4si a, v1sf b) { return b; }
v1df foo6 (v4si a, v1df b) { return b; }
v1tf foo7 (v4si a, v1tf b) { return b; }
