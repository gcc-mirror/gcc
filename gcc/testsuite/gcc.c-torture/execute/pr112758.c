/* PR rtl-optimization/112758 */

int a = -__INT_MAX__ - 1;

int
main ()
{
  if (-__INT_MAX__ - 1U == 0x80000000ULL)
    {
      unsigned long long b = 0xffff00ffffffffffULL;
      if ((b & a) != 0xffff00ff80000000ULL)
	__builtin_abort ();
    }
  return 0;
}
