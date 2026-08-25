/* PR target/127055 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4.1 -mno-sse4.2 -masm=att" } */
/* { dg-final { scan-assembler "roundps\[ \t]\+\\\$11," } } */
/* { dg-final { scan-assembler "roundps\[ \t]\+\\\$10," } } */
/* { dg-final { scan-assembler "roundps\[ \t]\+\\\$9," } } */
/* { dg-final { scan-assembler "roundpd\[ \t]\+\\\$11," } } */
/* { dg-final { scan-assembler "roundpd\[ \t]\+\\\$10," } } */
/* { dg-final { scan-assembler "roundpd\[ \t]\+\\\$9," } } */

float a[16], b[16];
double c[8], d[8];

void
test_truncf (void)
{
  int i;
  for (i = 0; i < 16; ++i)
    b[i] = __builtin_truncf (a[i]);
}

void
test_ceilf (void)
{
  int i;
  for (i = 0; i < 16; ++i)
    b[i] = __builtin_ceilf (a[i]);
}

void
test_floorf (void)
{
  int i;
  for (i = 0; i < 16; ++i)
    b[i] = __builtin_floorf (a[i]);
}

void
test_trunc (void)
{
  int i;
  for (i = 0; i < 8; ++i)
    d[i] = __builtin_trunc (c[i]);
}

void
test_ceil (void)
{
  int i;
  for (i = 0; i < 8; ++i)
    d[i] = __builtin_ceil (c[i]);
}

void
test_floor (void)
{
  int i;
  for (i = 0; i < 8; ++i)
    d[i] = __builtin_floor (c[i]);
}

