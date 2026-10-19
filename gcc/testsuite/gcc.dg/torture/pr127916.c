/* PR tree-optimization/127916 */
/* { dg-do run } */

/* The operands of these additions do not have the type of the result, so
   they are not saturating additions in that type.  */

__attribute__((noipa)) signed char
narrow (int a, int b)
{
  signed char r;
  if (__builtin_add_overflow (a, b, &r))
    return a < 0 ? -128 : 127;
  return r;
}

__attribute__((noipa)) int
mixed (int a, unsigned b)
{
  int r;
  if (__builtin_add_overflow (a, b, &r))
    return a < 0 ? -__INT_MAX__ - 1 : __INT_MAX__;
  return r;
}

__attribute__((noipa)) signed char
imm (signed char a)
{
  signed char r;
  if (__builtin_add_overflow (a, 200, &r))
    return a < 0 ? -128 : 127;
  return r;
}

/* A - 1 saturated in int, but converted to short.  */

__attribute__((noipa)) short
dec (int a)
{
  unsigned ua = a, r = ua - 1;
  if ((a & (int) -ua) < 0)
    r = 0x8000u;
  return r;
}

#define N 64
int x[N], y[N];
signed char z[N];

__attribute__((noipa)) void
narrow_loop (void)
{
  for (int i = 0; i < N; i++)
    {
      signed char r;
      if (__builtin_add_overflow (x[i], y[i], &r))
	r = x[i] < 0 ? -128 : 127;
      z[i] = r;
    }
}

/* These are saturating additions.  */

__attribute__((noipa)) signed char
imm_fits (signed char a)
{
  signed char r;
  if (__builtin_add_overflow (a, 9, &r))
    return a < 0 ? -128 : 127;
  return r;
}

#ifdef __BITINT_MAXWIDTH__
__attribute__((noipa)) _BitInt(32)
dec_bitint (int a)
{
  unsigned ua = a, r = ua - 1;
  if ((a & (int) -ua) < 0)
    r = 0x80000000u;
  return r;
}
#endif

int
main (void)
{
  if (narrow (-1, 200) != -128 || narrow (100, 100) != 127
      || narrow (1, 2) != 3)
    __builtin_abort ();
  if (mixed (-1, 0x80000000u) != __INT_MAX__
      || mixed (0, 0x80000000u) != __INT_MAX__ || mixed (1, 2) != 3)
    __builtin_abort ();
  if (imm (-1) != -128 || imm (-100) != 100)
    __builtin_abort ();
  if (dec (-__INT_MAX__ - 1) != -32768 || dec (5) != 4)
    __builtin_abort ();
  for (int i = 0; i < N; i++)
    {
      x[i] = i;
      y[i] = 100;
    }
  narrow_loop ();
  for (int i = 0; i < N; i++)
    if (z[i] != (i < 28 ? i + 100 : 127))
      __builtin_abort ();
  if (imm_fits (120) != 127 || imm_fits (-128) != -119)
    __builtin_abort ();
#ifdef __BITINT_MAXWIDTH__
  if (dec_bitint (-__INT_MAX__ - 1) != -__INT_MAX__ - 1
      || dec_bitint (5) != 4)
    __builtin_abort ();
#endif
  return 0;
}
