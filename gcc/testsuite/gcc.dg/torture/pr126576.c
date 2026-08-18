/* PR tree-optimization/126576 */
/* { dg-do run } */
/* { dg-add-options ieee } */

#if __DBL_MANT_DIG__ == 53 && __DBL_MAX_10_EXP__ == 308 \
    && __DBL_HAS_INFINITY__ && __FLT_EVAL_METHOD__ == 0
int v;

[[gnu::noipa]] void
bar (int x)
{
  v |= x;
}

[[gnu::noipa]] double
foo (double x)
{
  double a = x * x;
  if (__builtin_isinf (a) && !__builtin_isinf (x))
    bar (1);
  double b = a * 3.0;
  if (__builtin_isinf (b) && !__builtin_isinf (a))
    bar (2);
  return b;
}
#endif

int
main ()
{
#if __DBL_MANT_DIG__ == 53 && __DBL_MAX_10_EXP__ == 308 \
    && __DBL_HAS_INFINITY__ && __FLT_EVAL_METHOD__ == 0
  foo (1e154);
  if (v != 2)
    __builtin_abort ();
  v = 0;
  foo (1e152);
  if (v != 0)
    __builtin_abort ();
  foo (1e155);
  if (v != 1)
    __builtin_abort ();
#endif    
}
