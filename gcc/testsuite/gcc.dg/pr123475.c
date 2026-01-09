/* PR c/123475 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized -ftrivial-auto-var-init=zero" } */

int
foo (void)
{
  _Atomic int a = 1000;
  int b;
  b = a;	/* { dg-bogus "is used uninitialized" } */
  return b;
}
