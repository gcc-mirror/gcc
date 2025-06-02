/* PR target/120480 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

struct S { int a, b, c; } s;

void
foo (void)
{
  struct S t = s;
}
