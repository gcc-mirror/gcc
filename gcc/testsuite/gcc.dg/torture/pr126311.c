/* { dg-do run } */
/* PR tree-optimization/126311 */

/* Corner cases for the (t * u) / v fold: it must not reduce by a gcd
   that doesn't divide v.  */

#define UMAX   (__INT_MAX__ * 2U + 1)
#define DIV    (UMAX - 2)
#define T_MAX  (DIV / 3)	/* Largest t with t * 3 < DIV.  */

/* The result is always 0.  Don't reduce by 6, which abs (INT_MIN)
   wrapping would give as the gcd.  */
__attribute__((noipa)) int
f1 (int t)
{
  return (t * 6) / (-__INT_MAX__ - 1);
}

/* t * 3 can't wrap and stays below DIV, so the result is always 0.
   Don't reduce by 3, which doesn't divide DIV.  */
__attribute__((noipa)) unsigned
f2 (unsigned x)
{
  unsigned t = x % (T_MAX + 1);
  return (t * 3u) / DIV;
}

int
main ()
{
  if (f1 (__INT_MAX__ / 6) != 0)
    __builtin_abort ();
  if (f1 (-(__INT_MAX__ / 6)) != 0)
    __builtin_abort ();
  if (f2 (T_MAX) != 0)
    __builtin_abort ();
  if (f2 (T_MAX - 1) != 0)
    __builtin_abort ();
  return 0;
}
