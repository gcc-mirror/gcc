/* { dg-do compile } */

struct S { int y; };

struct V
{
  int x;
  #pragma omp declare mapper (bar: struct S s: s) map(s)	/* { dg-error "'#pragma omp declare mapper' not at file or block scope" } */
  /* { dg-error "expected end of line before '\\(' token" "" { target *-*-* } .-1 }  */
  #pragma omp declare mapper (struct V z : z) map(z)		/* { dg-error "'#pragma omp declare mapper' not at file or block scope" } */
  /* { dg-error "expected end of line before '\\(' token" "" { target *-*-* } .-1 }  */
};

