/* PR target/95125 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512f -mprefer-vector-width=512 " } */

extern float f[8];
extern double d[8];

void
float_truncate_512 (void)
{
  f[0] = d[0];
  f[1] = d[1];
  f[2] = d[2];
  f[3] = d[3];
  f[4] = d[4];
  f[5] = d[5];
  f[6] = d[6];
  f[7] = d[7];
}

void
float_extend_512 (void)
{
  d[0] = f[0];
  d[1] = f[1];
  d[2] = f[2];
  d[3] = f[3];
  d[4] = f[4];
  d[5] = f[5];
  d[6] = f[6];
  d[7] = f[7];
}



/* { dg-final { scan-assembler-times "vcvtps2pd" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2ps" 1 } } */
