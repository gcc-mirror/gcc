/* PR tree-optimization/127760 */
/* { dg-do run } */

/* The overflow flag of __builtin_add_overflow is computed for the type of
   the result, so none of these is a saturating addition in the type of
   a + b.  */

__attribute__((noipa)) unsigned
narrow_ne (unsigned a, unsigned b)
{
  unsigned char r;
  if (__builtin_add_overflow (a, b, &r))
    return -1u;
  return a + b;
}

__attribute__((noipa)) unsigned
narrow_eq (unsigned a, unsigned b)
{
  unsigned char r;
  int ovf = __builtin_add_overflow (a, b, &r);
  unsigned res = -1u;
  if (!ovf)
    res = a + b;
  return res;
}

__attribute__((noipa)) unsigned
sgn (unsigned a, unsigned b)
{
  int r;
  if (__builtin_add_overflow (a, b, &r))
    return -1u;
  return a + b;
}

#define N 64
unsigned x[N], y[N], z[N];

__attribute__((noipa)) void
narrow_loop (void)
{
  for (int i = 0; i < N; i++)
    {
      unsigned char r;
      z[i] = __builtin_add_overflow (x[i], y[i], &r) ? -1u : x[i] + y[i];
    }
}

int
main (void)
{
  if (narrow_ne (200, 100) != -1u || narrow_ne (100, 100) != 200)
    __builtin_abort ();
  if (narrow_eq (200, 100) != -1u || narrow_eq (100, 100) != 200)
    __builtin_abort ();
  if (sgn (0x80000000u, 0) != -1u || sgn (1, 2) != 3)
    __builtin_abort ();
  for (int i = 0; i < N; i++)
    {
      x[i] = 200;
      y[i] = i;
    }
  narrow_loop ();
  for (int i = 0; i < N; i++)
    if (z[i] != (i < 56 ? 200 + i : -1u))
      __builtin_abort ();
  return 0;
}
