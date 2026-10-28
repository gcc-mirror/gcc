/* PR middle-end/116135 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */

__attribute__((noipa)) int
mul31 (unsigned _BitInt(31) x, unsigned _BitInt(31) y,
       unsigned _BitInt(31) *res)
{
  return __builtin_mul_overflow (x, y, res);
}

__attribute__((noipa)) int
mul31_wide (unsigned long long x, unsigned long long y,
            unsigned _BitInt(31) *res)
{
  return __builtin_mul_overflow (x, y, res);
}

int
main ()
{
  unsigned _BitInt(31) res;

  if (!mul31 (((unsigned _BitInt(31)) 1) << 30, 2, &res) || res != 0)
    __builtin_abort ();

  if (mul31 (((unsigned _BitInt(31)) 1) << 15,
	     (((unsigned _BitInt(31)) 1) << 15) - 1, &res)
      || res != ((((unsigned _BitInt(31)) 1) << 30)
		 - (((unsigned _BitInt(31)) 1) << 15)))
    __builtin_abort ();

  if (!mul31_wide (1ULL << 63, 1ULL << 63, &res) || res != 0)
    __builtin_abort ();

  if (!mul31_wide (1ULL << 31, 1, &res) || res != 0)
    __builtin_abort ();

  if (mul31_wide (1234, 5678, &res)
      || res != (unsigned _BitInt(31)) (1234ULL * 5678ULL))
    __builtin_abort ();

  return 0;
}
