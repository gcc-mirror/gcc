/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -msse2" } */


float f[2];
double d[2];

void
test_float_truncate (void)
{
  for (int i = 0; i < 2; i++)
    f[i] = d[i];
}

/* { dg-final { scan-assembler "\tv?cvtpd2psx?" } } */

void
test_float_extend (void)
{
  for (int i = 0; i < 2; i++)
    d[i] = f[i];
}

/* { dg-final { scan-assembler "\tv?cvtps2pd" } } */
