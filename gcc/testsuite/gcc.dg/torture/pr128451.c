/* { dg-do run } */
/* { dg-additional-options "-Wno-psabi" } */

typedef signed char V __attribute__((vector_size (256)));
V x, y, r, m;

__attribute__((noipa)) void
f (void)
{
  r = __builtin_shuffle (x, m);
}

__attribute__((noipa)) void
g (void)
{
  r = __builtin_shuffle (x, y, (V) { -1, -128, 2 });
}

int
main ()
{
  for (int i = 0; i < 256; i++)
    {
      x[i] = i & 0x7f;
      y[i] = -1;
      m[i] = i;
    }
  m[0] = -1;
  m[1] = -128;
  f ();
  if (r[0] != x[255] || r[1] != x[128] || r[2] != x[2])
    __builtin_abort ();
  g ();
  if (r[0] != x[255] || r[1] != x[128] || r[2] != x[2])
    __builtin_abort ();
  return 0;
}
