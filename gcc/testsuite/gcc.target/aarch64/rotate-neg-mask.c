/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { *-*-* } { "-mgeneral-regs-only" } } */

/* Keep SIMD enabled so that SHIFT_COUNT_TRUNCATED is false.  */
__attribute__((noipa)) unsigned int
rotate32 (unsigned int x, unsigned int n)
{
  return __builtin_stdc_rotate_right (x, 0U - (n & 31U));
}

__attribute__((noipa)) unsigned long long
rotate64 (unsigned long long x, unsigned long long n)
{
  return __builtin_stdc_rotate_right (x, 0ULL - (n & 63ULL));
}

__attribute__((noipa)) unsigned long long
rotate64_count32 (unsigned long long x, unsigned int n)
{
  return __builtin_stdc_rotate_right (x, 0U - (n & 63U));
}

int
main (void)
{
  unsigned int expected32 = 1;
  unsigned long long expected64 = 1;
  for (unsigned int n = 0; n < 128; ++n)
    {
      if (rotate32 (1, n) != expected32
	  || rotate64 (1, n) != expected64
	  || rotate64_count32 (1, n) != expected64)
	__builtin_abort ();
      expected32 = (expected32 << 1) | (expected32 >> 31);
      expected64 = (expected64 << 1) | (expected64 >> 63);
    }
  return 0;
}
