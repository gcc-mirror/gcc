/* PR target/114605 */
/* { dg-do run } */
/* { dg-options "-O0" } */

typedef struct { const float *a; int b, c; float *d; } S;

__attribute__((noipa)) void
bar (void)
{
}

__attribute__((noinline, optimize (2))) static void
foo (S *e)
{
  const float *f;
  float *g;
  float h[4] = { 0.0, 0.0, 1.0, 1.0 };
  if (!e->b)
    f = h;
  else
    f = e->a;
  g = &e->d[0];
  __builtin_memcpy (g, f, sizeof (float) * 4);
  bar ();
  if (!e->b)
    if (g[0] != 0.0 || g[1] != 0.0 || g[2] != 1.0 || g[3] != 1.0)
      __builtin_abort ();
}

int
main ()
{
  float d[4];
  S e = { .d = d };
  foo (&e);
  return 0;
}
