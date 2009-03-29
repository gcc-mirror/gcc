/* Test for constant expressions: invalid null pointer constants in
   various contexts (make sure NOPs are not inappropriately
   stripped).  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void *p = (__SIZE_TYPE__)(void *)0; /* { dg-error "without a cast" } */
struct s { void *a; } q = { (__SIZE_TYPE__)(void *)0 }; /* { dg-error "without a cast" } */
void *
f (void)
{
  void *r;
  r = (__SIZE_TYPE__)(void *)0; /* { dg-error "without a cast" } */
  return (__SIZE_TYPE__)(void *)0; /* { dg-error "without a cast" } */
}
void g (void *); /* { dg-message "but argument is of type" } */
void
h (void)
{
  g ((__SIZE_TYPE__)(void *)0); /* { dg-error "without a cast" } */
}
void g2 (int, void *); /* { dg-message "but argument is of type" } */
void
h2 (void)
{
  g2 (0, (__SIZE_TYPE__)(void *)0); /* { dg-error "without a cast" } */
}
