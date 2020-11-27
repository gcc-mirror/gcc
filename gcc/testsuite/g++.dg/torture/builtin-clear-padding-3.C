/* PR libstdc++/88101 */

struct D { int a; int : 24; int b : 8; };
struct E {};
struct F { char c, d, e; };
struct G : public D, E, F { int f; } g1, g2;

__attribute__((noipa)) void
foo (G *g)
{
  g->a = -1; g->b = -1; g->c = -1; g->d = -1; g->e = -1; g->f = -1;
}

int
main ()
{
  __builtin_memset (&g2, -1, sizeof (g2));
  foo (&g1);
  foo (&g2);
  __builtin_clear_padding (&g2);
  if (__builtin_memcmp (&g1, &g2, sizeof (g1)))
    __builtin_abort ();
  return 0;
}
