/* Test for C99 declarations in for loops.  Test constraints.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  /* See comments in check_for_loop_decls (c-decl.c) for the presumptions
     behind these tests.  */
  int j = 0;
  for (int i = 1, bar (void); i <= 10; i++) /* { dg-bogus "warning" "warning in place of error" } */
    j += i;
  /* { dg-error "bar" "function in for loop" { target *-*-* } 12 } */
  for (static int i = 1; i <= 10; i++) /* { dg-bogus "warning" "warning in place of error" } */
    j += i;
  /* { dg-error "static" "static in for loop" { target *-*-* } 15 } */
  for (extern int i; j <= 500; j++) /* { dg-bogus "warning" "warning in place of error" } */
    j += 5;
  /* { dg-error "extern" "extern in for loop" { target *-*-* } 18 } */
  for (enum { FOO } i = FOO; i < 10; i++) /* { dg-bogus "warning" "warning in place of error" } */
    j += i;
  /* { dg-error "FOO" "enum value in for loop" { target *-*-* } 21 } */
  for (enum BAR { FOO } i = FOO; i < 10; i++) /* { dg-bogus "warning" "warning in place of error" } */
    j += i;
  /* { dg-error "FOO" "enum value in for loop" { target *-*-* } 24 } */
  /* { dg-error "BAR" "enum tag in for loop" { target *-*-* } 24 } */
}
