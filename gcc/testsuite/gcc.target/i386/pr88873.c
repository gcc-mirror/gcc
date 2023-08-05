/* { dg-do compile  { target int128 } } */
/* { dg-options "-O2 -march=cascadelake" } */

typedef struct { double x, y; } s_t;

s_t foo (s_t a, s_t b, s_t c)
{
  return (s_t) { __builtin_fma(a.x, b.x, c.x), __builtin_fma (a.y, b.y, c.y) };
}

/* { dg-final { scan-assembler-times "vpunpcklqdq" 3 } } */
/* { dg-final { scan-assembler "vunpckhpd" } } */
/* { dg-final { scan-assembler-not "rsp" } } */
