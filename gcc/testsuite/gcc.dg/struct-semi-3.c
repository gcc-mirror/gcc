/* Test diagnostics for missing and extra semicolons in structures.
   Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

struct s0 { ; }; /* { dg-error "extra semicolon in struct or union specified" } */
/* { dg-error "struct has no members" "empty" { target *-*-* } 7 } */
struct s1 {
  int a;
  ; /* { dg-error "extra semicolon in struct or union specified" } */
  int b;
};
struct s2 {
  ; /* { dg-error "extra semicolon in struct or union specified" } */
  int c
}; /* { dg-error "no semicolon at end of struct or union" } */
struct s3 {
  int d
}; /* { dg-error "no semicolon at end of struct or union" } */
