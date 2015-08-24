/* PR 20249 */

/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-arcs" } */
/* { dg-require-profiling "-fprofile-generate" } */

extern int *g (int x, void* y);
extern void fg (long long x, int y);

static void
ff (int y, long long z)
{
  fg (z, 1);
}

void
f ()
{
  g (42, ff);
}
