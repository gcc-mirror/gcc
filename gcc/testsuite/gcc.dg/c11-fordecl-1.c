/* Test for C99 declarations in for loops.  Test constraints are diagnosed for
   C11.  Based on c99-fordecl-2.c.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void
foo (void)
{
  int j = 0;
  for (int i = 1, bar (void); i <= 10; i++) /* { dg-error "bar" } */
    j += i;

  for (static int i = 1; i <= 10; i++) /* /* { dg-error "static" } */
    j += i;

  for (extern int i; j <= 500; j++) /* { dg-error "extern" } */
    j += 5;

  for (enum { FOO } i = FOO; i < 10; i++) /* { dg-error "FOO" } */
    j += i;

  for (enum BAR { FOO } i = FOO; i < 10; i++) /* { dg-error "FOO" } */
    /* { dg-error "BAR" "enum tag in for loop" { target *-*-* } .-1 } */
    j += i;
  for (typedef int T;;) /* { dg-error "non-variable" } */
    ;
}
