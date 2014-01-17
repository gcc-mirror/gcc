/* PR tree-optimization/58346 */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct U {};
static struct U b[1] = { };
extern void bar (struct U);

void
foo (void)
{
  bar (b[0]);
}

void
baz (void)
{
  foo ();
}
