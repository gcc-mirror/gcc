/* { dg-do compile } */
/* { dg-options "" } */

struct S { int i, j, k; };

void
foo (void)
{
  struct S s = { .i = 1, .j = 2, .l = 4}; /* { dg-error "34:unknown field .l. specified in initializer" } */
}
