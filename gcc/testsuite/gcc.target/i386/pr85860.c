/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -fno-guess-branch-probability -flive-range-shrinkage -mbmi2" } */

int a, b, c, d, e;

extern int bar(void);

__int128
foo (unsigned g, int h, long i, __int128 j, short k, __int128 l)
{
  unsigned __int128 m = j;
  do
    {
      j %= 5;
      c = c >> (m & 31);
      e = __builtin_sub_overflow (b, 0, &m);
      d = bar ();
      l *= __builtin_mul_overflow_p ((unsigned) d, ~(unsigned __int128) 1,
				     (unsigned __int128) 0);
    }
  while (a);
  return m + j + k + l;
}
