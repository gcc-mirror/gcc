/* PR target/92125 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx" } */

extern float f[4];
extern double d[4];

void
float_truncate_256 (void)
{
  f[0] = d[0];
  f[1] = d[1];
  f[2] = d[2];
  f[3] = d[3];
}

void
float_extend_256 (void)
{
  d[0] = f[0];
  d[1] = f[1];
  d[2] = f[2];
  d[3] = f[3];
}

/* { dg-final { scan-assembler-times "vcvtps2pd" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps" 1 } } */
