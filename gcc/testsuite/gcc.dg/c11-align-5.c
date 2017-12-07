/* Test C11 alignment support.  Test invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void foo (int []);
void bar1 (int [_Alignas (double) 10]); /* { dg-error "expected expression before" } */
void bar2 (int [static _Alignas (double) 10]); /* { dg-error "expected expression before" } */
void bar3 (int [static const _Alignas (double) 10]); /* { dg-error "expected expression before" } */
void bar4 (int [const _Alignas (double) 10]); /* { dg-error "expected expression before" } */
void bar5 (int [_Alignas (0) *]); /* { dg-error "expected expression before" } */

void foo (int a[_Alignas (0) 10]) { } /* { dg-error "expected expression before" } */

void
test (void)
{
  int *_Alignas (long) p; /* { dg-error "expected" } */
  int *const _Alignas (long) *q; /* { dg-error "expected" } */
  struct s { int n; };
  __builtin_offsetof (struct s _Alignas (int), n); /* { dg-error "expected" } */
  __typeof (long double _Alignas (0)) e; /* { dg-error "expected" } */
  sizeof (int _Alignas (int)); /* { dg-error "specified for type name" } */
  _Alignas (int _Alignas (float)) int t; /* { dg-error "expected" } */
  __builtin_types_compatible_p (signed _Alignas (0), unsigned); /* { dg-error "expected" } */
  int a[_Alignas (int) 10]; /* { dg-error "expected expression before" } */
  int b[10];
  foo (b);
}
