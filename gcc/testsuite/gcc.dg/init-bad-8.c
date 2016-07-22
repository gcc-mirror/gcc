/* { dg-do compile } */
/* { dg-options "" } */

struct S { int i, j, k; };

void
foo (void)
{
  struct S s = { .i = 1, .j = 2, .l = 4}; /* { dg-error "35: .struct S. has no member named .l." } */
}
