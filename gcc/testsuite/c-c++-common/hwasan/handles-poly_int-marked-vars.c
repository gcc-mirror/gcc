/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-march=armv8.6-a+sve -fsanitize-address-use-after-scope" } */

#include <arm_sve.h>

__attribute__((noinline, noclone)) int
foo (char *a)
{
  int i, j = 0;
  asm volatile ("" : "+r" (a) : : "memory");
  for (i = 0; i < 12; i++)
    j += a[i];
  return j;
}

int
main ()
{
  int i, j = 0;
  for (i = 0; i < 4; i++)
    {
      char a[12];
      __SVInt8_t freq;
      /* Just do something with that `freq` variable so that the compiler
	 doesn't optimise its use away.  */
      if (__builtin_bcmp (&freq, a, 10))
	j += 1;
      __builtin_memset (a, 0, sizeof (a));
      j += foo (a);
    }
  return j;
}

/* Just ensure this compiles without giving an ICE.
   This is the equivalent of PR 97696 but for HWASAN.  HWASAN can handle
   poly_int sized variables, and this testcase ensures that we don't ICE when
   given them.  */
