/* PR middle-end/85090 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mtune=intel -masm=att" } */

typedef signed char V __attribute__((vector_size (64)));

V
f1 (V x, int y)
{
  x[0] = y;
  return x;
}

V
f2 (V x, int y)
{
  x[15] = y;
  return x;
}

V
f3 (V x, int y)
{
  x[22] = y;
  return x;
}

V
f4 (V x, int y)
{
  x[59] = y;
  return x;
}

/* { dg-final { scan-assembler-times "vpbroadcastb\t" 4 } } */
