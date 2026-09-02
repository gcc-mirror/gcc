/* PR tree-optimization/127149 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O3" } */

typedef unsigned _BitInt(7) B;

[[gnu::noipa]] void
foo (B *restrict y, const B *restrict x)
{
  for (unsigned i = 0; i < 16; ++i)
    y[i] = __builtin_popcountg (x[i]);
}

[[gnu::noipa]] void
bar (B *restrict y, const B *restrict x)
{
  for (unsigned i = 0; i < 16; ++i)
    y[i] = __builtin_ctzg (x[i], 8);
}

int
main ()
{
  B x[16], y[16], z, w[16];
  B e[16] = { 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 };
  B f[16] = { 8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0 };
  for (unsigned i = 0; i < 16; ++i)
    x[i] = i;
  z = ~0;
  __builtin_clear_padding (&z);
  for (unsigned i = 0; i < 16; ++i)
    ((unsigned char *) &x[0])[i] |= ~*(unsigned char *) &z;
  foo (y, x);
  bar (w, x);
  for (unsigned i = 0; i < 16; ++i)
    if (y[i] != e[i] || w[i] != f[i])
    __builtin_abort ();
}
