/* { dg-do compile } */
/* { dg-options "-O -w" } */

void typedef (*cb) (void);

static void
bar (cb *fp)
{
  (*fp) ();
}

void
foo (void)
{
  bar (foo);
}
