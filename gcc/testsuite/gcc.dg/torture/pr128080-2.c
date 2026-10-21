/* PR tree-optimization/128080 */
/* { dg-do run } */
/* { dg-additional-options "-Wno-psabi" } */

typedef signed char V __attribute__((vector_size (256)));

/* 0, 257, 258, ..., 511.  */
#define M1(n) ((n) == 0 ? 0 : 256 + (n))
#define M4(n) M1 (n), M1 (n + 1), M1 (n + 2), M1 (n + 3)
#define M16(n) M4 (n), M4 (n + 4), M4 (n + 8), M4 (n + 12)
#define M64(n) M16 (n), M16 (n + 16), M16 (n + 32), M16 (n + 48)

V x, b, r;

__attribute__((noipa)) void
f (void)
{
  /* c = { x[0], b[1], b[2], ..., b[255] }.  */
  V c = __builtin_shufflevector (x, b, M64 (0), M64 (64), M64 (128), M64 (192));
  /* d = { c[1], c[0], ... } = { b[1], x[0], ... }.  */
  V m = { 1, 0 };
  r = __builtin_shuffle (c, m);
}

int
main ()
{
  for (int i = 0; i < 256; i++)
    {
      x[i] = 1;
      b[i] = 2;
    }
  f ();
  if (r[0] != 2 || r[1] != 1)
    __builtin_abort ();
  return 0;
}
