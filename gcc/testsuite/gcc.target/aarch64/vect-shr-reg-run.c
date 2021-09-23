/* { dg-do run } */
/* { dg-options "-O3 -march=armv8.2-a" } */

#include "vect-shr-reg.c"

int
main(void)
{
  int64_t a[16];
  int64_t b[16];
  int64_t c[17];

  uint64_t ua[16];
  uint64_t ub[16];
  uint64_t uc[17];

  int64_t res_a[16];
  uint64_t res_ua[16];

  int i;

  /* Set up inputs.  */
  for (i = 0; i < 16; i++)
    {
      b[i] = -2;
      c[i] = 34;
      ub[i] = 0xffffffffffffffff;
      uc[i] = 52;
    }

  /* Set up reference values.  */
  for (i = 0; i < 16; i++)
    {
      res_a[i] = -1LL;
      res_ua[i] = 0x0fffLL;
    }

  /* Do the shifts.  */
  f (ua, ub, uc);
  g (a, b, c);

  /* Compare outputs against reference values.  */
  for (i = 0; i < 16; i++)
    {
      if (a[i] != res_a[i])
	__builtin_abort ();

      if (ua[i] != res_ua[i])
	__builtin_abort ();
    }

  return 0;
}
