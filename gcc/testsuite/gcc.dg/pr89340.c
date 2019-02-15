/* PR c/89340 */
/* { dg-do compile } */
/* { dg-options "" } */

void bar (void)
{
  __attribute__((weak)) void foo () {}	/* { dg-error "weak declaration of 'foo' must be public" } */
  foo ();
}
