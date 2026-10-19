/* PR tree-optimization/127357 */
/* PR tree-optimization/127916 */
/* { dg-do run } */

/* ((X ^ SUM) & (X ^ 2)) < 0 tests X < 0 && SUM >= 0, which is not the
   overflow test of X + 1.  */

__attribute__((noipa)) int
s_sat (int x)
{
  int sum = (int) ((unsigned) x + 1u);
  if (((x ^ sum) & (x ^ 2)) < 0)
    return -(int) (x < 0) ^ __INT_MAX__;
  return sum;
}

static signed char
idiom (signed char x)
{
  signed char sum = (signed char) ((unsigned char) x + 1);
  signed char c = (signed char) ((x ^ sum) & (x ^ 2));
  signed char satval = (signed char) (-(signed char) (x < 0) ^ 127);
  return c < 0 ? satval : sum;
}

#define N 256
signed char in[N], out[N];

__attribute__((noipa)) void
loop (void)
{
  for (int i = 0; i < N; i++)
    out[i] = idiom (in[i]);
}

/* (X ^ -3) < 0 tests X >= 0, so this is a saturating addition.  */

__attribute__((noipa)) int
sat (int x)
{
  int sum = (int) ((unsigned) x + 1u);
  if (((x ^ sum) & (x ^ -3)) < 0)
    return -(int) (x < 0) ^ __INT_MAX__;
  return sum;
}

int
main (void)
{
  if (s_sat (__INT_MAX__) != -__INT_MAX__ - 1
      || s_sat (-1) != -__INT_MAX__ - 1 || s_sat (1) != 2)
    __builtin_abort ();
  for (int i = 0; i < N; i++)
    in[i] = i - 128;
  loop ();
  for (int i = 0; i < N; i++)
    if (out[i] != (in[i] == -1 ? -128 : (signed char) (in[i] + 1)))
      __builtin_abort ();
  if (sat (__INT_MAX__) != __INT_MAX__ || sat (-1) != 0 || sat (5) != 6)
    __builtin_abort ();
  return 0;
}
