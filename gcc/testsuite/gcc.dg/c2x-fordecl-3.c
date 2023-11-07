/* Test for C99 declarations in for loops.  Test constraints are diagnosed with
   -Wc11-c23-compat for C23.  Based on c99-fordecl-2.c.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

void
foo (void)
{
  int j = 0;
  for (int i = 1, bar (void); i <= 10; i++) /* { dg-warning "bar" } */
    j += i;

  for (static int i = 1; i <= 10; i++) /* /* { dg-warning "static" } */
    j += i;

  for (extern int i; j <= 500; j++) /* { dg-warning "extern" } */
    j += 5;

  for (enum { FOO } i = FOO; i < 10; i++) /* { dg-warning "FOO" } */
    j += i;

  for (enum BAR { FOO } i = FOO; i < 10; i++) /* { dg-warning "FOO" } */
    /* { dg-warning "BAR" "enum tag in for loop" { target *-*-* } .-1 } */
    j += i;
  for (typedef int T;;) /* { dg-warning "non-variable" } */
    ;
}
