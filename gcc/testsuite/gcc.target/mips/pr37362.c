/* mips*-sde-elf doesn't have 128-bit long doubles.  */
/* { dg-do compile { target { ! mips*-sde-elf } } } */
/* { dg-options "-march=mips64r2 -mabi=n32" } */

typedef float TFtype __attribute__((mode(TF)));

TFtype
__powitf (TFtype x, int m)
{
  unsigned int n = m < 0 ? -m : m;
  TFtype y = n % 2 ? x : 1;
  while (n >>= 1)
    {
      x = x * x;
      if (n % 2)
	y = y * x;
    }
  return m < 0 ? 1/y : y;
}

