/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse4.1 -mno-sse4.2 -masm=att" } */
/* { dg-final { scan-assembler "roundps\[ \t]\+\\\$8," } } */
/* { dg-final { scan-assembler "roundpd\[ \t]\+\\\$8," } } */

float a[16], b[16];
double c[8], d[8];

void
test_roundevenf (void)
{
  int i;
  for (i = 0; i < 16; ++i)
    b[i] = __builtin_roundevenf (a[i]);
}

void
test_roundeven (void)
{
  int i;
  for (i = 0; i < 8; ++i)
    d[i] = __builtin_roundeven (c[i]);
}

