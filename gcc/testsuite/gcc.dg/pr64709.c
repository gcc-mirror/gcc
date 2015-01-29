/* PR c/64709 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

struct S { int a, b; };
void
foo (void)
{
  struct S s[] = { { 1, 2 }, { 0 } }; /* { dg-bogus "missing initializer for field" } */
}
