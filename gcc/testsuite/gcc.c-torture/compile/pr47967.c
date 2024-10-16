/* PR tree-optimization/47967 */
/* { dg-additional-options "-std=gnu17" } */
/* { dg-require-effective-target untyped_assembly } */

extern void abort (void);
static void bar ();

void
foo ()
{
  bar (1);
}

static void
bar (double i)
{
  if (i)
    abort ();
}
