/* { dg-do compile } */
/* { dg-options "-Wnonnull" } */

void foox (char*, ...) __attribute__ ((nonnull (1)));
#define foo(p) foox (p, "p is null") /* { dg-warning "null argument" } */

void baz (void)
{
  foo (0); /* { dg-message "note: in expansion" } */
}
