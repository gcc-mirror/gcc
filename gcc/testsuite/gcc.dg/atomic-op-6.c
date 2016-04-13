/* Test we don't generate bogus warnings.  */
/* PR c/69407 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra" } */

void
foo (int *p, int a)
{
  __atomic_fetch_add (&p, a, 0); /* { dg-bogus "value computed is not used" } */
  __atomic_add_fetch (&p, a, 0); /* { dg-bogus "value computed is not used" } */
}
