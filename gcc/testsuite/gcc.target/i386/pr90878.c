/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake" } */

union ieee754_float
  {
    float f;

    struct
      {
	unsigned int mantissa:23;
	unsigned int exponent:8;
	unsigned int negative:1;
      } ieee;
};

double
foo (float f)
{
  union ieee754_float u;
  u.f = f;
  u.ieee.negative = 0;
  return u.f;
}

/* { dg-final { scan-assembler-not "vcvtss2sd\[^\\n\]*\\\(%.sp\\\)" } } */
