/* { dg-do compile } */

struct S;
#pragma omp declare reduction (+:struct S:omp_out.s += omp_in.s) /* { dg-error "invalid use of undefined type" } */
struct S { int s; };
#pragma omp declare reduction (*:struct S:omp_out.s *= omp_in.s)
