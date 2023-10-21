/* { dg-require-effective-target untyped_assembly } */
/* { dg-additional-options "-std=gnu89" } */

void bar ();

void
foo (void *x, short y)
{
  bar (x, y + 1);
}

void
bar (x, y)
  void *x;
  char *y;
{
  baz (y);
}
