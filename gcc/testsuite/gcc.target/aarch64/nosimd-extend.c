/* { dg-do assemble } */
/* { dg-options "-O2 -march=armv8-a+nosimd -save-temps" } */

/* umov and smov are Advanced SIMD instructions, so they must not be used
   to move a 16-bit value out of an FP register when SIMD is disabled.  */

unsigned int
zext (_Float16 x)
{
  unsigned short s;
  __builtin_memcpy (&s, &x, sizeof (s));
  return s;
}

int
sext (_Float16 x)
{
  short s;
  __builtin_memcpy (&s, &x, sizeof (s));
  return s;
}

/* { dg-final { scan-assembler-not {\tumov\t} } } */
/* { dg-final { scan-assembler-not {\tsmov\t} } } */
