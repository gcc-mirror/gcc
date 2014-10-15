/* PR c/63480 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

/* Test that we don't warn about initializing with { }.  */

struct S { int a, b, c; } s = { };

void
foo (void)
{
  struct S s = { }; 
  struct S s2 = (struct S){ };
}
