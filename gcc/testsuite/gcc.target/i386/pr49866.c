/* PR target/49866 */
/* { dg-do assemble { target lp64 } } */
/* { dg-options "-O2 -mcmodel=large"  } */

void fn (void *, int, int);
int fn2 (void);
void baz (int);

static void
foo (void *x, int y)
{
  int i;
  for (i = 0; i < y; i++)
    fn (x, fn2 (), i);
}

void
bar (int u, int v, int w, void *x)
{
  baz (u);
  foo (x, w);
  baz (u);
}
