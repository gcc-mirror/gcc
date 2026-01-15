/* PR tree-optimization/120322 */
/* { dg-do run } */
/* { dg-additional-options "-fno-early-inlining" } */

int a, b, c;

void
foo (int e)
{
  if (e > 0)
    while (b)
      ;
}

void
bar (unsigned short e)
{
  foo (e);
}

static void
baz (short e)
{
  bar (e);
  c = a;
}

int
main ()
{
  if (sizeof (int) > sizeof (short))
    baz (-1);
}
