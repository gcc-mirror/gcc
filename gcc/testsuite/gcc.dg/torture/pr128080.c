/* PR tree-optimization/128080 */
/* { dg-do run } */
/* The permutations are merged only when optimizing.  */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-additional-options "-Wno-psabi" } */

typedef signed char V __attribute__((vector_size (256)));

/* 255, 254, ..., 0.  */
#define R1(n) (255 - (n))
#define R4(n) R1 (n), R1 (n + 1), R1 (n + 2), R1 (n + 3)
#define R16(n) R4 (n), R4 (n + 4), R4 (n + 8), R4 (n + 12)
#define R64(n) R16 (n), R16 (n + 16), R16 (n + 32), R16 (n + 48)

V x, b, r;

__attribute__((noipa)) void
f (void)
{
  /* c[i] = b[255 - i].  */
  V c = __builtin_shufflevector (b, b, R64 (0), R64 (64), R64 (128), R64 (192));
  /* Index -1 selects element 511 of { x, c }, i.e. c[255] = b[0].  */
  V m = { 0, -1 };
  r = __builtin_shuffle (x, c, m);
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
  if (r[0] != 1 || r[1] != 2)
    __builtin_abort ();
  return 0;
}
