/* Test C1X alignment support.  Test invalid code.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x -pedantic-errors" } */

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
  int a[_Alignas (int) 10]; /* { dg-error "expected expression before" } */
  int b[10];
  foo (b);
}
